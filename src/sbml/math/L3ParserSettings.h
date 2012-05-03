/**
 * @file    L3ParserSettings.h
 * @brief   Definition of the level 3 infix-to-mathml parser settings.
 * @author  Lucian Smith
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
 * ---------------------------------------------------------------------- -->
 *
 * @class L3ParserSettings
 * @brief A helper class for controlling the behavior of the text-string 
 * formula parser.
 *
 * @htmlinclude not-sbml-warning.html
 *
 * The function
 * @if clike SBML_parseL3FormulaWithSettings()@endif@if csharp SBML_parseL3FormulaWithSettings()@endif@if python libsbml.parseL3FormulaWithSettings()@endif@if java <code><a href="libsbml.html#parseL3FormulaWithSettings(java.lang.String)">libsbml.parseL3FormulaWithSettings()</a></code>@endif@~,
 * along with its variants 
 * @if clike SBML_parseFormula()@endif@if csharp SBML_parseFormula()@endif@if python libsbml.parseFormula()@endif@if java <code><a href="libsbml.html#parseFormula(java.lang.String)">libsbml.parseFormula()</a></code>@endif@~
 * and
 * @if clike SBML_parseL3FormulaWithModel()@endif@if csharp SBML_parseL3FormulaWithModel()@endif@if python libsbml.parseL3FormulaWithModel()@endif@if java <code><a href="libsbml.html#parseL3FormulaWithModel(java.lang.String)">libsbml.parseL3FormulaWithModel()</a></code>@endif@~,
 * are the interfaces to a parser for mathematical formulas expressed as
 * text strings.  The parser converts the text-string formulas into
 * Abstract Syntax Trees (ASTs), represented in libSBML using ASTNode
 * objects. Compared to the parser implemented by the function
 * @if clike SBML_parseFormula()@endif@if csharp SBML_parseFormula()@endif@if python libsbml.parseFormula()@endif@if java <code><a href="libsbml.html#parseFormula(java.lang.String)">libsbml.parseFormula()</a></code>@endif@~,
 * which was designed primarily for converting the mathematical formula
 * strings in SBML Level&nbsp;1, the "L3" variant of the parser accepts an
 * extended formula syntax.  It also has a number of configurable behaviors.
 * This class (L3ParserSettings) is an object used to communicate the
 * configuration settings with callers.
 *
 * The following aspects of the parser are configurable:
 * <ul>
 * <li> The function @c log with a single argument (&quot;<code>log(x)</code>&quot;) 
 * can be parsed as <code>log10(x)</code>, <code>ln(x)</code>, or treated
 * as an error, as desired.
 * <li> Multiple unary minuses in a row (e.g., &quot;<code>- -3</code>&quot;)
 * can be turned into a single minus in the AST representation, or the multiple
 * minuses can be preserved.
 * <li> Parsing of units can be turned on and off.
 * <li> The string @c avogadro can be parsed as a MathML @em csymbol or
 * as an identifier.
 * <li> A Model object may optionally be provided to the parser using
 * the variant function call @if clike  SBML_parseL3FormulaWithModel()@endif@if csharp  SBML_parseL3FormulaWithModel()@endif@if python  libsbml.SBML_parseL3FormulaWithModel()@endif@if java  SBML_parseL3FormulaWithModel()@endif@~.
 * or stored in a L3ParserSettings object passed to the variant function
 * @if clike SBML_parseL3FormulaWithSettings()@endif@if csharp SBML_parseL3FormulaWithSettings()@endif@if python libsbml.parseL3FormulaWithSettings()@endif@if java <code><a href="libsbml.html#parseL3FormulaWithSettings(java.lang.String)">libsbml.parseL3FormulaWithSettings()</a></code>@endif@~.
 * When a Model object is provided, identifiers (values of type @c SId)
 * from that model are used in preference to pre-defined MathML
 * definitions.  More precisely, the Model entities whose identifiers will
 * shadow identical symbols in the mathematical formula are: Species,
 * Compartment, Parameter, Reaction, and SpeciesReference.  For instance,
 * if the parser is given a Model containing a Species with the identifier
 * &quot;<code>pi</code>&quot;, and the formula to be parsed is
 * &quot;<code>3*pi</code>&quot;, the MathML produced will contain the
 * construct <code>&lt;ci&gt; pi &lt;/ci&gt;</code> instead of the
 * construct <code>&lt;pi/&gt;</code>.
 * <li> Similarly, when a Model object is provided, @c SId values of
 * user-defined functions present in the Model will be used preferentially
 * over pre-defined MathML functions.  For example, if the passed-in Model
 * contains a FunctionDefinition with the identifier
 * &quot;<code>sin</code>&quot;, that function will be used instead of the
 * predefined MathML function <code>&lt;sin/&gt;</code>.
 * </ul>
 *
 * To obtain the default configuration values, callers can use the function
 * @if clike SBML_getDefaultL3ParserSettings()@endif@if csharp SBML_getDefaultL3ParserSettings()@endif@if python libsbml.SBML_getDefaultL3ParserSettings()@endif@if java SBML_getDefaultL3ParserSettings()@endif@~.
 * To change the configuration, callers can create an L3ParserSettings
 * object, set the desired characteristics using the methods
 * provided, and pass that object to
 * @if clike SBML_parseL3FormulaWithSettings()@endif@if csharp SBML_parseL3FormulaWithSettings()@endif@if python libsbml.parseL3FormulaWithSettings()@endif@if java <code><a href="libsbml.html#parseL3FormulaWithSettings(java.lang.String)">libsbml.parseL3FormulaWithSettings()</a></code>@endif@~.
 *
 * @if clike @see SBML_parseL3FormulaWithSettings()@endif@~
 * @if csharp @see SBML_parseL3FormulaWithSettings()@endif@~
 * @if python @see libsbml.SBML_parseL3FormulaWithSettings()@endif@~
 * @if java @see SBML_parseL3FormulaWithSettings()@endif@~
 * @if clike @see SBML_parseL3Formula()@endif@~
 * @if csharp @see SBML_parseL3Formula()@endif@~
 * @if python @see libsbml.SBML_parseL3Formula()@endif@~
 * @if java @see SBML_parseL3Formula()@endif@~
 * @if clike @see SBML_parseL3FormulaWithModel()@endif@~
 * @if csharp @see SBML_parseL3FormulaWithModel()@endif@~
 * @if python @see libsbml.SBML_parseL3FormulaWithModel()@endif@~
 * @if java @see SBML_parseL3FormulaWithModel()@endif@~
 */

#ifndef L3ParserSettings_h
#define L3ParserSettings_h

#include <sbml/common/libsbml-namespace.h>
#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


/** 
  * The l3p_log_type enum defines three options:
  * @li L3P_PARSE_LOG_AS_LOG10 (0):  parse 'log(x)' as the log base-10 of x, or 'log10(x)'.
  * @li L3P_PARSE_LOG_AS_LN (1): parse 'log(x)' as the natural log of x, or 'ln(x)'.
  * @li L3P_PARSE_LOG_AS_ERROR (2): refuse to parse 'log(x)' at all, and set an error message 
      telling the user to use 'log10(x)', 'ln(x)', or 'log(base, x)' instead.
  */
typedef enum {L3P_PARSE_LOG_AS_LOG10=0,
              L3P_PARSE_LOG_AS_LN=1,
              L3P_PARSE_LOG_AS_ERROR=2
} l3p_log_type ;


#define L3P_COLLAPSE_UNARY_MINUS true
#define L3P_EXPAND_UNARY_MINUS   false

#define L3P_PARSE_UNITS  true
#define L3P_NO_UNITS false

#define L3P_AVOGADRO_IS_CSYMBOL true
#define L3P_AVOGADRO_IS_NAME    false

#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN

class Model;

class LIBSBML_EXTERN L3ParserSettings
{
private:
  const Model* mModel;
  l3p_log_type mParselog;
  bool mCollapseminus;
  bool mParseunits;
  bool mAvoCsymbol;

public:

  /**
   * Creates a new L3ParserSettings object with default values.
   * 
   * This is the default constructor for the L3ParserSettings object.  It
   * sets the Model to @c NULL and other settings to @c
   * L3P_PARSE_LOG_AS_LOG10, @c L3P_EXPAND_UNARY_MINUS, @c L3P_PARSE_UNITS,
   * and @c L3P_AVOGADRO_IS_CSYMBOL.
   */
  L3ParserSettings();


  /**
   * Creates a new L3ParserSettings object with specific values for all
   * possible settings.
   *
   * @param model a Model object to be used for disambiguating identifiers
   * 
   * @param parselog a flag that controls how the parser will handle
   * apparent calls to logarithm operators in formulas
   *
   * @param collapseminus a flag that controls how the parser will handle
   * multiple minus signs in a row
   *
   * @param parseunits a flag that controls how the parser will handle
   * apparent references to units of measurement associated with raw
   * numbers in a formula
   *
   * @param avocsymbol a flag that controls how the parser will handle
   * the appearance of the symbol @c avogadro in a formula
   */
  L3ParserSettings(Model* model, l3p_log_type parselog,
                   bool collapseminus, bool parseunits, bool avocsymbol);


  /**
   * Destroys this L3ParserSettings object.
   */
  ~L3ParserSettings();


  /**
   * Sets the model reference in this L3ParserSettings object.
   *
   * When a Model object is provided, identifiers (values of type @c SId)
   * from that model are used in preference to pre-defined MathML
   * definitions.  More precisely, the Model entities whose identifiers will
   * shadow identical symbols in the mathematical formula are: Species,
   * Compartment, Parameter, Reaction, and SpeciesReference.  For instance,
   * if the parser is given a Model containing a Species with the identifier
   * &quot;<code>pi</code>&quot;, and the formula to be parsed is
   * &quot;<code>3*pi</code>&quot;, the MathML produced will contain the
   * construct <code>&lt;ci&gt; pi &lt;/ci&gt;</code> instead of the
   * construct <code>&lt;pi/&gt;</code>.
   * Similarly, when a Model object is provided, @c SId values of
   * user-defined functions present in the Model will be used preferentially
   * over pre-defined MathML functions.  For example, if the passed-in Model
   * contains a FunctionDefinition with the identifier
   * &quot;<code>sin</code>&quot;, that function will be used instead of the
   * predefined MathML function <code>&lt;sin/&gt;</code>.
   *
   * @param model a Model object to be used for disambiguating identifiers
   *
   * @warning This does @em not copy the Model object.  This means that
   * modifications made to the object after invoking this method may affect
   * parsing behavior.
   *
   * @see getModel()
   * @see unsetModel()
   */
  void setModel(const Model* model);


  /**
   * Returns the Model object referenced by this L3ParserSettings object.
   *
   * @see setModel()
   * @see unsetModel()
   */
  const Model* getModel() const;


  /**
   * Sets the Model reference in this L3ParserSettings object to @c NULL.
   *
   * @see setModel()
   * @see getModel()
   */
  void unsetModel();


  /**
   * Sets what to do with infix strings with the function 'log' with one
   * argument, according to the provided type.
   *
   * @param type One of three options to set how 'log' with one argument is interpreted:
   * @li L3P_PARSE_LOG_AS_LOG10 (0)
   * @li L3P_PARSE_LOG_AS_LN (1)
   * @li L3P_PARSE_LOG_AS_ERROR (2)
   */
  void setParseLog(l3p_log_type type);


  /**
   * Returns the current setting indicating what to do with infix strings
   * with the function 'log' with one argument.
   *
   * @return One of three options to set how 'log' with one argument is interpreted:
   * @li L3P_PARSE_LOG_AS_LOG10 (0)
   * @li L3P_PARSE_LOG_AS_LN (1)
   * @li L3P_PARSE_LOG_AS_ERROR (2)
   */
  l3p_log_type getParseLog() const;


  /**
   * Sets whether to collapse unary minuses (L3P_COLLAPSE_UNARY_MINUS,
   * true) or whether to leave them expanded (L3P_EXPAND_UNARY_MINUS,
   * false).  Unary minuses found in infix will always be translated into
   * an AST node of type AST_MINUS in the latter case, and subsequent pairs
   * will disappear entirely in the former case, as well as numbers
   * (AST_INTEGER, AST_REAL, AST_REAL_E, and AST_RATIONAL) nodes being
   * given negative values.
   *
   * @param collapseminus A boolean indicating whether to collapse unary
   * minuses (L3P_COLLAPSE_UNARY_MINUS, true) or whether to leave them
   * expanded (L3P_EXPAND_UNARY_MINUS, false).
   */
  void setCollapseMinus(bool collapseminus);


  /**
   * Returns whether the L3ParserSettings object is set to collapse unary
   * minuses (L3P_COLLAPSE_UNARY_MINUS, true) or whether to leave them
   * expanded (L3P_EXPAND_UNARY_MINUS, false).
   *
   * @return A boolean indicating whether unary minuses are set to be
   * collapsed (L3P_COLLAPSE_UNARY_MINUS, true) or whether they are set to
   * be left expanded (L3P_EXPAND_UNARY_MINUS, false).
   */
  bool getCollapseMinus() const;


  /**
   * Sets up this L3ParserSettings object to create AST nodes that are
   * capable of being used unmodified in SBML Level 2 documents: this means
   * that infix that associates a unit definition with a number is treated
   * as an error, and that the string 'avogadro' is converted to an AST of
   * type AST_NAME instead of AST_NAME_AVOGADRO.
   */
  void targetL2();


  /**
   * Sets up this L3ParserSettings object to create AST nodes that are
   * targetted to the capabilities of SBML Level 3 documents: this means
   * that infix that associates a unit definition with a number is parsed
   * correctly and used to set the 'unit' of the AST, and that the string
   * 'avogadro' is converted to an AST of type AST_NAME_AVOGADRO instead of
   * AST_NAME.
   */
  void targetL3();


  /**
   * Returns a boolean indicating whether this L3ParserSettings object is
   * set up to produce AST nodes appropriate for inclusion in SBML Level 2
   * documents: units on numbers are treated as errors, and 'avogadro' is
   * interpreted as AST_NAME and not AST_NAME_AVOGADRO.  (Both must be
   * true, or this routine will return 'false'.)
   */
  bool getTargetL2() const;


  /**
   * Returns a boolean indicating whether this L3ParserSettings object is
   * set up to produce AST nodes that take full advantage of properties
   * present in SBML Level 2 documents: units are properly interpreted, and
   * 'avogadro' is interpreted as AST_NAME_AVOGADRO and not AST_NAME.
   * (Both must be true, or this routine will return 'false'.)
   */
  bool getTargetL3() const;


  /**
   * Sets whether to parse units associated with number (L3P_PARSE_UNITS,
   * true) or whether to treat units as errors (L3P_NO_UNITS, false).  SBML
   * Level 3 allows the setting of a 'sbml:units' attribute on MathML <cn>
   * elements, which is settable in infix as the unit definition ID string
   * used after a number.  Some examples include: "4 mL" (for integers),
   * "2.01 Hz" (for reals), "3.1e-6 M" (for real numbers using e-notation),
   * and "(5/8) inches" (for rational numbers).  To produce a valid SBML
   * file, the UnitDefinition must exist in the final model, or the unit
   * must be obtained from the list of pre-defined units in Table 2 of the
   * SBML specification.
   *
   * @param units A boolean indicating whether to parse units
   * (L3P_PARSE_UNITS, true) or whether to treat these strings as errors
   * (L3P_NO_UNITS, false).
   */
  void setParseUnits(bool units);


  /**
   * Returns whether the L3ParserSettings object is set to parse units
   * (L3P_PARSE_UNITS, true) or whether to treat these strings as errors
   * (L3P_NO_UNITS, false).
   *
   * @return A boolean indicating whether units are set to be parsed
   * (L3P_PARSE_UNITS, true) or whether they are set to be treated as errors
   * (L3P_NO_UNITS, false).
   */
  bool getParseUnits() const;


  /**
   * Sets whether to translate the string "avogadro" (in any
   * capitalization) into an AST node of type AST_NAME_AVOGADRO
   * (L3P_AVOGADRO_IS_CSYMBOL, true) or whether to translate it into an AST
   * of type AST_NAME (L3P_AVOGADRO_IS_NAME, false).  'Avogadro' is a
   * pre-defined csymbol in SBML Level 3, but was not defined in SBML Level
   * 2, so this should be set to 'false' when parsing infix destined for
   * SBML Level 2 documents.
   *
   * @param units A boolean indicating whether to parse "avogadro" as a
   * csymbol (L3P_AVOGADRO_IS_CSYMBOL, true) or to parse it as a normal
   * AST_NAME (L3P_AVOGADRO_IS_NAME, false).
   */
  void setAvogadroCsymbol(bool l2only);


  /**
   * Returns whether the L3ParserSettings object is set to parse "avogadro"
   * as a csymbol (L3P_AVOGADRO_IS_CSYMBOL, true) or whether to parse it as
   * a normal AST_NAME (L3P_AVOGADRO_IS_NAME, false).
   *
   * @return A boolean indicating whether "avogadro" is to be parsed as a
   * csymbol (L3P_AVOGADRO_IS_CSYMBOL, true) or whether to parse it as a
   * normal AST_NAME (L3P_AVOGADRO_IS_NAME, false).
   */
  bool getAvogadroCsymbol() const;
};


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


LIBSBML_EXTERN
L3ParserSettings_t *
L3ParserSettings_create ();


LIBSBML_EXTERN
void
L3ParserSettings_free (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_setModel (L3ParserSettings_t * settings, const Model_t * model);


LIBSBML_EXTERN
const Model_t *
L3ParserSettings_getModel (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_unsetModel (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_setParseLog (L3ParserSettings_t * settings, l3p_log_type type);


LIBSBML_EXTERN
l3p_log_type
L3ParserSettings_getParseLog (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_setCollapseMinus (L3ParserSettings_t * settings, int flag);


LIBSBML_EXTERN
int
L3ParserSettings_getCollapseMinus (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_setTargetL2 (L3ParserSettings_t * settings);


LIBSBML_EXTERN
int
L3ParserSettings_getTargetL2 (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_setTargetL3 (L3ParserSettings_t * settings);


LIBSBML_EXTERN
int
L3ParserSettings_getTargetL3 (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_setParseUnits (L3ParserSettings_t * settings, int flag);


LIBSBML_EXTERN
int
L3ParserSettings_getParseUnits (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_setAvogadroCsymbol (L3ParserSettings_t * settings, int flag);


LIBSBML_EXTERN
int
L3ParserSettings_getAvogadroCsymbol (L3ParserSettings_t * settings);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif

#endif /* L3ParserSettings_h */
