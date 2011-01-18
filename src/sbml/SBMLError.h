/**
 * @file    SBMLError.h
 * @brief   Represents SBML errors and other diagnostics
 * @author  Michael Hucka
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class SBMLError
 * @brief Representation of errors, warnings and other diagnostics
 *
 * @htmlinclude not-sbml-warning.html
 *
 * When a libSBML operation on SBML content results in an error, or when
 * there is something wrong with the SBML content, the problems are
 * reported as SBMLError objects.  These are generally stored in an
 * SBMLErrorLog object; this log object, in turn, is kept in the
 * SBMLDocument object containing the SBML content.  Applications can
 * obtain the list of logged errors using SBMLDocument::getErrorLog() and
 * then use the methods provided by SBMLErrorLog to access individual
 * SBMLError objects.  (Note that despite the word "error" in the name,
 * SBMLError objects are used to represent not only "true" errors, but also
 * warnings and some informational diagnostics.  The name is a historical
 * hold-over from early versions of libSBML, in which the object really was
 * only used to report errors.)
 *
 * @if clike
 * Each SBMLError object instance has an identification number that
 * identifies the nature of the problem.  This "error id" number will be up
 * to five digits long, and it will be listed in one of two enumerations:
 * <a class="el" href="#SBMLErrorCode_t"> SBMLErrorCode_t</a> (described <a
 * class="el" href="#SBMLErrorCode_t"> below</a>) or @link
 * XMLError::XMLErrorCode_t XMLErrorCode_t @endlink (described in the
 * documentation for the class XMLError).  The former enumeration contains
 * all the SBML validation rule numbers listed in the appendices of the
 * SBML specification documents, as well as some additional
 * libSBML-specific error codes.
 * @endif@if java
 * Each SBMLError object instance has an identification number that
 * identifies the nature of the problem.  This "error id" number will be up
 * to five digits long, and it will come from one of two sets of static
 * integer constants defined in the interface class <code><a
 * href="libsbmlConstants.html"> libsbmlConstants</a></code>: either the
 * SBML error identifiers <a class="el" href="#SBMLErrorCode_t"> (described
 * below)</a> or the XML error identifiers (described in the documentation
 * for the class <code><a href="XMLError.html"> XMLError</a></code>).  The
 * former set of constants includes all the SBML validation rule numbers
 * listed in the appendices of the SBML specification documents, as well as
 * some additional libSBML-specific error codes.
 * @endif@if python
 * Each SBMLError object instance has an identification number that
 * identifies the nature of the problem.  This "error id" number will be up
 * to five digits long, and it will come from one
 * of two sets of static integer constants defined in
 * the interface class @link libsbml libsbml@endlink: either the SBML
 * error identifiers <a
 * class="el" href="#SBMLErrorCode_t"> (described below)</a> or the XML
 * error identifiers (described in the documentation for the class XMLError).
 * The former set of constants
 * includes all the SBML validation rule numbers listed in the appendices
 * of the SBML specification documents, as well as some additional
 * libSBML-specific error codes.
 * @endif
 * 
 * Error codes are useful mainly for software.  For human readers,
 * SBMLError also includes text messages that describe the nature of a
 * given problem.  The messages can be accessed using
 * SBMLError::getShortMessage() and SBMLError::getMessage().  The former
 * provides a brief one-line description of the issue, while
 * SBMLError::getMessage() provides a more detailed text, including (if
 * appropriate) references to sections of the SBML specifications where
 * relevant topics are discussed.  These text strings are suitable for
 * displaying to human users.
 *
 * @if clike
 * An SBMLError object also contains a category code; its value may be
 * retrieved using the method SBMLError::getCategory().  Category values
 * are drawn from the enumeration <a class="el"
 * href="#SBMLErrorCategory_t">SBMLErrorCategory_t</a> described below.
 * Categories are used to partition errors into distinct conceptual groups.
 * This is principally used by the libSBML validation system to group
 * classes of validation checks.  For example, 
 * @link SBMLErrorCategory_t#LIBSBML_CAT_IDENTIFIER_CONSISTENCY LIBSBML_CAT_IDENTIFIER_CONSISTENCY@endlink
 * is the category for tests that check identifier consistency;
 * @link SBMLErrorCategory_t#LIBSBML_CAT_MATHML_CONSISTENCY LIBSBML_CAT_MATHML_CONSISTENCY@endlink
 * is the category for MathML consistency checking; and
 * so on.
 * @endif@if java
 * An SBMLError object also contains a category code; its value may be
 * retrieved using the method SBMLError::getCategory().  Category values
 * are drawn from a set of static integer constants
 * defined in <code><a href="libsbmlConstants.html">libsbmlConstants</a></code>,
 * and having names beginning with the characters
 * <code>LIBSBML_CAT_</code>.  The list of possible codes is described in a
 * separate section below.  Categories are used to partition errors into
 * distinct conceptual groups.  This is principally used by the libSBML
 * validation system to group classes of validation checks.  For example,
 * @link SBMLErrorCategory_t#LIBSBML_CAT_IDENTIFIER_CONSISTENCY LIBSBML_CAT_IDENTIFIER_CONSISTENCY@endlink
 * is the category for tests that check identifier consistency;
 * @link SBMLErrorCategory_t#LIBSBML_CAT_MATHML_CONSISTENCY LIBSBML_CAT_MATHML_CONSISTENCY@endlink
 * is the category for MathML consistency checking; and
 * so on.
 * @endif@if python
 * An SBMLError object also contains a category code; its value may be
 * retrieved using the method SBMLError::getCategory().  Category values
 * are drawn from a set of static integer constants
 * defined in @link libsbml libsbml@endlink and having names beginning with the characters
 * <code>LIBSBML_CAT_</code>.  The list of possible codes is described in a
 * separate section below.  Categories are used to partition errors into
 * distinct conceptual groups.  This is principally used by the libSBML
 * validation system to group classes of validation checks.  For example,
 * @link SBMLErrorCategory_t#LIBSBML_CAT_IDENTIFIER_CONSISTENCY LIBSBML_CAT_IDENTIFIER_CONSISTENCY@endlink
 * is the category for tests that check identifier consistency;
 * @link SBMLErrorCategory_t#LIBSBML_CAT_MATHML_CONSISTENCY LIBSBML_CAT_MATHML_CONSISTENCY@endlink
 * is the category for MathML consistency checking; and
 * so on.
 * @endif
 *
 * In addition, SBMLError also has a severity code.  Its value may be
 * retrieved using the method SBMLError::getSeverity().  The possible
 * severity values are the same as those reported by @if clike XMLError.@endif@if python XMLError.@endif@if java <code><a href="XMLError.html">XMLError</a></code>.@endif 
 * Severity levels currently range from informational
 * (@link XMLErrorSeverity_t#LIBSBML_SEV_INFO LIBSBML_SEV_INFO@endlink)
 * to fatal errors
 * (@link XMLErrorSeverity_t#LIBSBML_SEV_FATAL LIBSBML_SEV_FATAL@endlink).
 * They can be
 * used by an application to evaluate how serious a given problem
 * is. 
 *
 * Finally, SBMLError records the line and column near where the problem
 * occurred in the SBML content.  The values may be retrieved using the
 * methods SBMLError::getLine() and SBMLError::getColumn().  We say "near",
 * because a lot of factors affect how accurate the line/column information
 * ultimately is.  For example, different XML parsers have different
 * conventions for which line and column number they report for a
 * particular problem (which makes a difference when a problem involves an
 * opening XML tag on one line and a closing tag on another line).  In some
 * situations, some parsers report invalid line and/or column numbers
 * altogether.  If this occurs, libSBML sets the line and/or column number
 * in the SBMLError object to the the value of the maximum unsigned long
 * integer representable on the platform where libSBML is running.  (This
 * is equal to the constant named <code>ULONG_MAX</code> in C and C++.)
 * The probability that a true line or column number in an SBML model would
 * equal this value is vanishingly small; thus, if an application
 * encounters these values in an XMLError object, it can assume no valid
 * line/column number could be provided by libSBML in that situation.
 *
 * @if clike
 * <h3><a class="anchor" name="SBMLErrorCode_t">SBMLErrorCode_t</a></h3>
 *
 * SBMLErrorCode_t is an enumeration of all SBML-level error, warning and
 * informational diagnostic codes.  Every SBMLError object has an error
 * code value that can be either a value from this enumeration, or a value
 * from the @link XMLError::XMLErrorCode_t XMLErrorCode_t @endlink
 * enumeration (see the documentation for XMLError).  The latter values
 * apply when the error or warning signifies a basic XML issue rather than
 * an SBML issue per se.  The values of SBMLErrorCode_t are distinguished
 * from those of @link XMLError::XMLErrorCode_t XMLErrorCode_t @endlink by
 * being numbered 10000 and higher, while the XML layer's codes are 9999 and
 * lower.  The method SBMLError::getErrorId() returns the error code of a
 * given SBMLError object instance.
 * 
 * The following is a table of the symbolic names of SBMLErrorCode_t values
 * and the meaning of each code.  In this table, the right-hand columns
 * titled "L1V1", "L1V2", etc. refer to Levels and Versions of the SBML
 * specifications, and the entries in each column refer to whether the
 * severity of the condition in that particular Level+Version of SBML.
 * The codes stand for the following:
 * 
 * @endif@if java <h3><a class="anchor" 
 * name="SBMLErrorCode_t">Error codes associated with SBMLError objects</a></h3>
 *
 * The error and warning codes returned by libSBML are listed in the table
 * below.  The method SBMLError::getErrorId() returns the error code of a
 * given SBMLError object instance.  In the libSBML Java language
 * interface, these error identifiers are currently
 * implemented as static integer constants defined in the interface class
 * <code><a href="libsbmlConstants.html">libsbmlConstants</a></code>.  This
 * is admittedly not an ideal approach from the standpoint of modern Java
 * programming, but it was necessary to work around the lack of
 * enumerations in Java prior to JDK 1.5.  Future versions of libSBML may
 * use a proper Java enumeration type to define the error identifiers.
 * 
 * In this table, the right-hand columns titled "L1V1", "L1V2", etc. refer
 * to Levels and Versions of the SBML specifications, and the entries in
 * each column refer to whether the severity of the condition in that
 * particular Level+Version of SBML.  The codes stand for the following:
 *
 * @endif@if python <h3><a class="anchor" 
 * name="SBMLErrorCode_t">Error codes associated with SBMLError objects</a></h3>
 *
 * The error and warning codes returned by libSBML are listed in the table
 * below.  The method SBMLError::getErrorId() returns the error code of a
 * given SBMLError object instance.  In the libSBML Python language
 * interface, these error identifiers are currently
 * implemented as static integer constants defined in the interface class
 * @link libsbml libsbml@endlink. 
 * 
 * In this table, the right-hand columns titled "L1V1", "L1V2", etc. refer
 * to Levels and Versions of the SBML specifications, and the entries in
 * each column refer to whether the severity of the condition in that
 * particular Level+Version of SBML.  The codes stand for the following:
 *
 * @endif
 *
 * <table cellspacing="1" cellpadding="2" border="0" class="normal-font">
 * <tr><td class="s-na">N</td><td>= Not applicable</td></tr>
 * <tr><td class="s-info">I</td><td>= Informational</td></tr>
 * <tr><td class="s-warning">W</td><td>= Warning</td></tr>
 * <tr><td class="s-error">E</td><td>= Error</td></tr>
 * <tr><td class="s-fatal">F</td><td>= Fatal</td></tr>
 * </table>
 *
 * The text shown in the "Meaning" is the text returned by the
 * SBMLError::getShortMessage() method on a given SBMLError object.  A
 * longer and (hopefully) clearer explanation of the issue is returned by
 * SBMLError::getMessage().
 *
 * @htmlinclude sbmlerror-table.html
 * 
 * @if clike <h3><a class="anchor" name="SBMLErrorCategory_t">SBMLErrorCategory_t</a></h3>
 *
 * SBMLErrorCategory_t is an enumeration of category codes for SBMLError
 * diagnostics.  The category can be retrieved from an SBMLError object
 * using the method SBMLError::getCategory().  These enumeration values are
 * distinct from (and in addition to) the @link
 * XMLError::XMLErrorCategory_t XMLErrorCategory_t @endlink codes used by
 * the parent XMLError object.  User programs receiving an SBMLError object
 * can use this distinction to check whether the error represents a
 * low-level XML problem or an SBML problem.
 *
 * The following table lists each possible value and a brief description of
 * its meaning.
 * 
 * @endif@if python <h3><a class="anchor" name="SBMLErrorCategory_t">Category codes associated with SBMLError objects</a></h3>
 *
 * As discussed above, each SBMLError object contains a value for a
 * category identifier, describing the type of issue that the SBMLError
 * object represents.  The category can be retrieved from an SBMLError
 * object using the method SBMLError::getCategory().  The following table
 * lists each possible value and a brief description of its meaning.
 *
 * As is the case with the error codes, in the libSBML Python language
 * interface, the category identifiers are currently implemented as static
 * integer constants defined in the interface class
 * @link libsbml libsbml@endlink. 
 *
 * The following table lists each possible value and a brief description of
 * its meaning.
 * 
 * @endif@if java <h3><a class="anchor"
 * name="SBMLErrorCategory_t">Category codes associated with SBMLError objects</a></h3>
 * 
 * As discussed above, each SBMLError object contains a value for a
 * category identifier, describing the type of issue that the SBMLError
 * object represents.  The category can be retrieved from an SBMLError
 * object using the method SBMLError::getCategory().  The following table
 * lists each possible value and a brief description of its meaning.
 * 
 * As is the case with the error codes, in the libSBML Java language
 * interface, the category identifiers are currently implemented as static
 * integer constants defined in the interface class
 * {@link libsbmlConstants}.
 *
 * The following table lists each possible value and a brief description of
 * its meaning.
 * 
 * @endif
 * 
 * <center>
 * <table width="90%" cellspacing="1" cellpadding="4" border="0"  class="text-table normal-font alt-row-colors">
 *  <tr style="background: lightgray" class="normal-font">
 *      <th>Enumerator</td>
 *      <th>Meaning</td>
 *  </tr>
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_SBML LIBSBML_CAT_SBML@endlink</td><td>General error not falling into
 * another category below.</td></tr> 
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_SBML_L1_COMPAT LIBSBML_CAT_SBML_L1_COMPAT@endlink</td><td>Category of errors
 * that can only occur during attempted translation from one Level/Version
 * of SBML to another.  This particular category applies to errors
 * encountered while trying to convert a model from SBML Level&nbsp;2 to SBML
 * Level&nbsp;1.</td></tr> 
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_SBML_L2V1_COMPAT LIBSBML_CAT_SBML_L2V1_COMPAT@endlink</td><td>Category of errors
 * that can only occur during attempted translation from one Level/Version
 * of SBML to another.  This particular category applies to errors
 * encountered while trying to convert a model to SBML Level&nbsp;2
 * Version&nbsp;1.</td></tr> 
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_SBML_L2V2_COMPAT LIBSBML_CAT_SBML_L2V2_COMPAT@endlink</td><td>Category of errors
 * that can only occur during attempted translation from one Level/Version
 * of SBML to another.  This particular category applies to errors
 * encountered while trying to convert a model to SBML Level&nbsp;2
 * Version&nbsp;2.</td></tr> 
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_GENERAL_CONSISTENCY LIBSBML_CAT_GENERAL_CONSISTENCY@endlink</td><td>Category of
 * errors that can occur while validating general SBML constructs.  With
 * respect to the SBML specification, these concern failures in applying
 * the validation rules numbered 2xxxx in the Level&nbsp;2 Versions&nbsp;2
 * and&nbsp;3 specifications.</td></tr>
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_IDENTIFIER_CONSISTENCY LIBSBML_CAT_IDENTIFIER_CONSISTENCY@endlink</td><td>Category of
 * errors that can occur while validating symbol identifiers in a model.
 * With respect to the SBML specification, these concern failures in
 * applying the validation rules numbered 103xx in the Level&nbsp;2
 * Versions&nbsp;2 and&nbsp;3 specifications.</td></tr>  
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_UNITS_CONSISTENCY LIBSBML_CAT_UNITS_CONSISTENCY@endlink</td><td>Category of
 * errors that can occur while validating the units of measurement on
 * quantities in a model.  With respect to the SBML specification, these
 * concern failures in applying the validation rules numbered 105xx in the
 * Level&nbsp;2 Versions&nbsp;2 and&nbsp;3 specifications.</td></tr> 
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_MATHML_CONSISTENCY LIBSBML_CAT_MATHML_CONSISTENCY@endlink</td><td>Category of
 * errors that can occur while validating MathML formulas in a model.  With
 * respect to the SBML specification, these concern failures in applying
 * the validation rules numbered 102xx in the Level&nbsp;2 Versions&nbsp;2
 * and&nbsp;3 specifications.</td></tr> 
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_SBO_CONSISTENCY LIBSBML_CAT_SBO_CONSISTENCY@endlink</td><td>Category of errors
 * that can occur while validating SBO identifiers in a model.  With
 * respect to the SBML specification, these concern failures in applying
 * the validation rules numbered 107xx in the Level&nbsp;2 Versions&nbsp;2
 * and&nbsp;3 specifications.</td></tr> 
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_OVERDETERMINED_MODEL LIBSBML_CAT_OVERDETERMINED_MODEL@endlink</td><td>Error in the
 * system of equations in the model: the system is overdetermined,
 * therefore violating a tenet of proper SBML.  With respect to the SBML
 * specification, this is validation rule #10601 in the SBML Level&nbsp;2
 * Versions&nbsp;2 and&nbsp;3 specifications.</td></tr> 
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_SBML_L2V3_COMPAT LIBSBML_CAT_SBML_L2V3_COMPAT@endlink</td><td>Category of errors
 * that can only occur during attempted translation from one Level/Version
 * of SBML to another.  This particular category applies to errors
 * encountered while trying to convert a model to SBML Level&nbsp;2
 * Version&nbsp;3.</td></tr> 
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_MODELING_PRACTICE LIBSBML_CAT_MODELING_PRACTICE@endlink</td><td>Category of
 * warnings about recommended good practices involving SBML and
 * computational modeling.  (These are tests performed by libSBML and do
 * not have equivalent SBML validation rules.)</td></tr> 
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_INTERNAL_CONSISTENCY LIBSBML_CAT_INTERNAL_CONSISTENCY@endlink</td><td>Category of
 * errors that can occur while validating libSBML's internal representation
 * of SBML constructs. (These are tests performed by libSBML and do
 * not have equivalent SBML validation rules.)</td></tr> 
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_SBML_L2V4_COMPAT LIBSBML_CAT_SBML_L2V4_COMPAT@endlink</td><td>Category of errors
 * that can only occur during attempted translation from one Level/Version
 * of SBML to another.  This particular category applies to errors
 * encountered while trying to convert a model to SBML Level&nbsp;2
 * Version&nbsp;4.</td></tr> 
 * <tr><td>@link XMLErrorCategory_t#LIBSBML_CAT_SBML_L3V1_COMPAT LIBSBML_CAT_SBML_L3V1_COMPAT@endlink</td><td>Category of errors
 * that can only occur during attempted translation from one Level/Version
 * of SBML to another.  This particular category applies to errors
 * encountered while trying to convert a model to SBML Level&nbsp;3
 * Version&nbsp;1.</td></tr> 
 *
 * </table>
 * </center>
 * 
 * @if clike
 * <h3><a class="anchor" name="SBMLErrorSeverity_t">SBMLErrorSeverity_t</a></h3>
 *
 * This is an enumeration of severity codes for SBMLError diagnostics.
 * User programs receiving an SBMLError object can use this distinction to
 * check whether the error represents a low-level XML problem or an SBML
 * problem.
 * 
 * In this verision of libSBML (3.3.x), there are no additional severity
 * codes in SBMLErrorSeverity_t beyond those defined in @link
 * XMLError::XMLErrorSeverity_t XMLErrorSeverity_t @endlink.
 * 
 * <hr>
 * @endif@if java <h3><a class="anchor"
 * name="SBMLErrorSeverity_t">Severity codes associated with SBMLError
 * objects</h3>
 *
 * In this verision of libSBML (3.3.x), there are no additional severity
 * codes beyond those defined by XMLError.  They are implemented as static
 * integer constants defined in the interface class <code><a
 * href="libsbmlConstants.html">libsbmlConstants</a></code>, and have names
 * beginning with <code>LIBSBML_SEV_</code>.
 * @endif@if python <h3><a class="anchor"
 * name="SBMLErrorSeverity_t">Severity codes associated with SBMLError
 * objects</h3>
 *
 * In this verision of libSBML (3.3.x), there are no additional severity
 * codes beyond those defined by XMLError.  They are implemented as static
 * integer constants defined in the interface class
 * @link libsbml libsbml@endlink, and have names
 * beginning with <code>LIBSBML_SEV_</code>.
 * @endif
 */

#ifndef SBMLError_h
#define SBMLError_h

#include <sbml/common/extern.h>
#include <sbml/xml/XMLError.h>
#include <sbml/SBMLNamespaces.h>


LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Codes for all SBML-level errors and warnings.
 *
 * These are distinguished from the XML layer (LIBLAX) error codes by being
 * numbered > 10000, while the XML layer's codes are < 9999.  Calling
 * programs may wish to check whether a given SBMLError object's error
 * identifier is actually from SBMLErrorCode_t or XMLError::XMLErrorCode_t.
 * This distinction corresponds to whether a given error represents a
 * low-level XML problem or an SBML problem.
 */
typedef enum
{
   UnknownError                          = 10000 /*!< Unknown internal libSBML error */
 , NotUTF8                               = 10101 /*!< Not UTF8 */
 , UnrecognizedElement                   = 10102 /*!< Unrecognized element */
 , NotSchemaConformant                   = 10103 /*!< Not conformant to SBML XML schema */
 , L3NotSchemaConformant                 = 10104 /*!< Not conformant to SBML XML schema */
 , InvalidMathElement                    = 10201 /*!< Invalid MathML */
 , DisallowedMathMLSymbol                = 10202 /*!< Disallowed MathML symbol */
 , DisallowedMathMLEncodingUse           = 10203 /*!< Disallowed use of MathML <code>encoding</code> attribute */
 , DisallowedDefinitionURLUse            = 10204 /*!< Disallowed use of MathML <code>definitionURL</code> attribute */
 , BadCsymbolDefinitionURLValue          = 10205 /*!< Invalid <code>&lt;csymbol&gt;</code> <code>definitionURL</code> attribute value */
 , DisallowedMathTypeAttributeUse        = 10206 /*!< Disallowed use of MathML <code>type</code> attribute */
 , DisallowedMathTypeAttributeValue      = 10207 /*!< Disallowed MathML <code>type</code> attribute value */
 , LambdaOnlyAllowedInFunctionDef        = 10208 /*!< Use of <code>&lt;lambda&gt;</code> not permitted outside of a <code>&lt;functionDefinition&gt;</code> */
 , BooleanOpsNeedBooleanArgs             = 10209 /*!< Non-boolean argument given to boolean operator */
 , NumericOpsNeedNumericArgs             = 10210 /*!< Non-numerical argument given to numerical operator */
 , ArgsToEqNeedSameType                  = 10211 /*!< Arguments to <code>&lt;eq&gt;</code> or <code>&lt;neq&gt;</code> have inconsistent data types */
 , PiecewiseNeedsConsistentTypes         = 10212 /*!< <code>&lt;piecewise&gt;</code> terms have inconsistent data types */
 , PieceNeedsBoolean                     = 10213 /*!< Second argument of <code>&lt;piece&gt;</code> must yield a boolean value */
 , ApplyCiMustBeUserFunction             = 10214 /*!< <code>&lt;ci&gt;</code> does not refer to a function definition */
 , ApplyCiMustBeModelComponent           = 10215 /*!< <code>&lt;ci&gt;</code>'s value is not a component in this model */
 , KineticLawParametersAreLocalOnly      = 10216 /*!< Cannot use <code>&lt;kineticLaw&gt;</code> parameter outside local scope */
 , MathResultMustBeNumeric               = 10217 /*!< Formula result is not a numerical value */
 , OpsNeedCorrectNumberOfArgs            = 10218 /*!< Incorrect number of arguments to operator */
 , InvalidNoArgsPassedToFunctionDef      = 10219 /*!< Incorrect number of arguments to function */
 , DisallowedMathUnitsUse                = 10220 /*!< Attribute <code>units</code> only permitted on cn element */
 , InvalidUnitsValue                     = 10221 /*!< Invalid value for <code>units</code> attribute */
 , DuplicateComponentId                  = 10301 /*!< Duplicate component identifier */
 , DuplicateUnitDefinitionId             = 10302 /*!< Duplicate unit definition identifier */
 , DuplicateLocalParameterId             = 10303 /*!< Duplicate local parameter identifier */
 , MultipleAssignmentOrRateRules         = 10304 /*!< Multiple rules for the same variable */
 , MultipleEventAssignmentsForId         = 10305 /*!< Multiple event assignments for the same variable */
 , EventAndAssignmentRuleForId           = 10306 /*!< <code>variable</code> value used in both event assignments and assignment rules */
 , DuplicateMetaId                       = 10307 /*!< Duplicate <code>metaid</code> identifier */
 , InvalidSBOTermSyntax                  = 10308 /*!< Invalid <code>sboTerm</code> value syntax */
 , InvalidMetaidSyntax                   = 10309 /*!< Invalid <code>metaid</code> value syntax */
 , InvalidIdSyntax                       = 10310 /*!< Invalid identifier syntax */
 , InvalidUnitIdSyntax                   = 10311 /*!< Invalid unit identifier syntax */
 , InvalidNameSyntax                     = 10312 /*!< Invalid name syntax */
 , MissingAnnotationNamespace            = 10401 /*!< Missing declaration of XML namespace for annotation */
 , DuplicateAnnotationNamespaces         = 10402 /*!< Multiple annotations using same XML namespace */
 , SBMLNamespaceInAnnotation             = 10403 /*!< Invalid use of SBML XML namespace in annotation */
 , MultipleAnnotations                   = 10404 /*!< Multiple annotation elements not allowed */
 , InconsistentArgUnits                  = 10501 /*!< Units of arguments to function call do not match function's definition */
 , InconsistentKineticLawUnitsL3         = 10503 /*!< Inconsistent <code>&lt;kineticLaw&gt;</code> units */
 , AssignRuleCompartmentMismatch         = 10511 /*!< Mismatched units in assignment rule for compartment */
 , AssignRuleSpeciesMismatch             = 10512 /*!< Mismatched units in assignment rule for species */
 , AssignRuleParameterMismatch           = 10513 /*!< Mismatched units in assignment rule for parameter */
 , AssignRuleStoichiometryMismatch       = 10514 /*!< Mismatched units in assignment rule for stoichiometry */
 , InitAssignCompartmenMismatch          = 10521 /*!< Mismatched units in initial assignment to compartment */
 , InitAssignSpeciesMismatch             = 10522 /*!< Mismatched units in initial assignment to species */
 , InitAssignParameterMismatch           = 10523 /*!< Mismatched units in initial assignment to parameter */
 , InitAssignStoichiometryMismatch       = 10524 /*!< Mismatched units in initial assignment to stoichiometry */
 , RateRuleCompartmentMismatch           = 10531 /*!< Mismatched units in rate rule for compartment */
 , RateRuleSpeciesMismatch               = 10532 /*!< Mismatched units in rate rule for species */
 , RateRuleParameterMismatch             = 10533 /*!< Mismatched units in rate rule for parameter */
 , RateRuleStoichiometryMismatch         = 10534 /*!< Mismatched units in rate rule for stoichiometry */
 , KineticLawNotSubstancePerTime         = 10541 /*!< Kinetic law units are not <code>substance</code>/<code>time</code> */
 , SpeciesInvalidExtentUnits             = 10542 /*!< Species units not consistent with extent */
 , DelayUnitsNotTime                     = 10551 /*!< Units of delay are not units of time */
 , EventAssignCompartmentMismatch        = 10561 /*!< Mismatched units in event assignment for compartment */
 , EventAssignSpeciesMismatch            = 10562 /*!< Mismatched units in event assignment for species */
 , EventAssignParameterMismatch          = 10563 /*!< Mismatched units in event assignment for parameter */
 , EventAssignStoichiometryMismatch      = 10564 /*!< Mismatched units in event assignment for stoichiometry */
 , PriorityUnitsNotDimensionless         = 10565 /*!< Priority units must be dimensionless */
 , OverdeterminedSystem                  = 10601 /*!< Model is overdetermined */
 , InvalidModelSBOTerm                   = 10701 /*!< Invalid <code>sboTerm</code> value for model */
 , InvalidFunctionDefSBOTerm             = 10702 /*!< Invalid <code>sboTerm</code> value for function definition */
 , InvalidParameterSBOTerm               = 10703 /*!< Invalid <code>sboTerm</code> value for parameter */
 , InvalidInitAssignSBOTerm              = 10704 /*!< Invalid <code>sboTerm</code> value for initial assignment */
 , InvalidRuleSBOTerm                    = 10705 /*!< Invalid <code>sboTerm</code> value for rule */
 , InvalidConstraintSBOTerm              = 10706 /*!< Invalid <code>sboTerm</code> value for constraint */
 , InvalidReactionSBOTerm                = 10707 /*!< Invalid <code>sboTerm</code> value for reaction */
 , InvalidSpeciesReferenceSBOTerm        = 10708 /*!< Invalid <code>sboTerm</code> value for species reference */
 , InvalidKineticLawSBOTerm              = 10709 /*!< Invalid <code>sboTerm</code> value for kinetic law */
 , InvalidEventSBOTerm                   = 10710 /*!< Invalid <code>sboTerm</code> value for event */
 , InvalidEventAssignmentSBOTerm         = 10711 /*!< Invalid <code>sboTerm</code> value for event assignment */
 , InvalidCompartmentSBOTerm             = 10712 /*!< Invalid <code>sboTerm</code> value for compartment */
 , InvalidSpeciesSBOTerm                 = 10713 /*!< Invalid <code>sboTerm</code> value for species */
 , InvalidCompartmentTypeSBOTerm         = 10714 /*!< Invalid <code>sboTerm</code> value for compartment type */
 , InvalidSpeciesTypeSBOTerm             = 10715 /*!< Invalid <code>sboTerm</code> value for species type */
 , InvalidTriggerSBOTerm                 = 10716 /*!< Invalid <code>sboTerm</code> value for event trigger */
 , InvalidDelaySBOTerm                   = 10717 /*!< Invalid <code>sboTerm</code> value for event delay */
 , NotesNotInXHTMLNamespace              = 10801 /*!< Notes not placed in XHTML namespace */
 , NotesContainsXMLDecl                  = 10802 /*!< XML declarations not permitted in notes */
 , NotesContainsDOCTYPE                  = 10803 /*!< XML <code>DOCTYPE</code> not permitted in notes */
 , InvalidNotesContent                   = 10804 /*!< Invalid notes content */
 , OnlyOneNotesElementAllowed            = 10805 /*!< Only one notes element allowed. */
 , InvalidNamespaceOnSBML                = 20101 /*!< Invalid XML namespace for SBML container */
 , MissingOrInconsistentLevel            = 20102 /*!< Missing or inconsistent value for <code>level</code> attribute */
 , MissingOrInconsistentVersion          = 20103 /*!< Missing or inconsistent value for <code>version</code> attribute */
 , PackageNSMustMatch                    = 20104 /*!< Invalid level/version on package namespace */
 , LevelPositiveInteger                  = 20105 /*!< <code>level</code> must be positive integer */
 , VersionPositiveInteger                = 20106 /*!< <code>version</code> must be positive integer */
 , AllowedAttributesOnSBML               = 20108 /*!< Invalid attribute on <code>&lt;sbml&gt;</code>  */
 , MissingModel                          = 20201 /*!< Missing model */
 , IncorrectOrderInModel                 = 20202 /*!< Incorrect ordering of components in model definition */
 , EmptyListElement                      = 20203 /*!< A given listOf___, if present, cannot be empty */
 , NeedCompartmentIfHaveSpecies          = 20204 /*!< Missing compartment in species definition */
 , OneOfEachListOf                       = 20205 /*!< Only one of each ListOf element allowed */
 , OnlyFuncDefsInListOfFuncDefs          = 20206 /*!< Only FunctionDefinitions allowed in ListOfFunctionDefinitions */
 , OnlyUnitDefsInListOfUnitDefs          = 20207 /*!< Only UnitDefinitions allowed in ListOfUnitDefinitions */
 , OnlyCompartmentsInListOfCompartments  = 20208 /*!< Only Compartments allowed in ListOfCompartments */
 , OnlySpeciesInListOfSpecies            = 20209 /*!< Only Species allowed in ListOfSpecies */
 , OnlyParametersInListOfParameters      = 20210 /*!< Only parameters allowed in ListOfParameters */
 , OnlyInitAssignsInListOfInitAssigns    = 20211 /*!< Only InitialAssignments allowed in ListOfInitialAssignments */
 , OnlyRulesInListOfRules                = 20212 /*!< Only Rules allowed in ListOfRules */
 , OnlyConstraintsInListOfConstraints    = 20213 /*!< Only Constraints allowed in ListOfConstraints */
 , OnlyReactionsInListOfReactions        = 20214 /*!< Only Reactions allowed in ListOfReactions */
 , OnlyEventsInListOfEvents              = 20215 /*!< Only Events allowed in ListOfEvents */
 , L3ConversionFactorOnModel             = 20216 /*!< ConversionFactor must be parameter */
 , L3TimeUnitsOnModel                    = 20217 /*!< Invalid value of <code>timeUnits</code> on <code>&lt;model&gt;</code> */
 , L3VolumeUnitsOnModel                  = 20218 /*!< Invalid value of <code>volumeUnits</code> on <code>&lt;model&gt;</code> */
 , L3AreaUnitsOnModel                    = 20219 /*!< Invalid value of <code>areaUnits</code> on <code>&lt;model&gt;</code> */
 , L3LengthUnitsOnModel                  = 20220 /*!< Invalid value of <code>lengthUnits</code> on <code>&lt;model&gt;</code> */
 , L3ExtentUnitsOnModel                  = 20221 /*!< Invalid value of <code>extentUnits</code> on <code>&lt;model&gt;</code> */
 , AllowedAttributesOnModel              = 20222 /*!< Invalid attribute on <code>&lt;model&gt;</code> */
 , AllowedAttributesOnListOfFuncs        = 20223 /*!< Invalid attribute on <code>&lt;listOfFunctionDefinitions&gt;</code> */
 , AllowedAttributesOnListOfUnitDefs     = 20224 /*!< Invalid attribute on <code>&lt;listOfUnitDefinitions&gt;</code> */
 , AllowedAttributesOnListOfComps        = 20225 /*!< Invalid attribute on <code>&lt;listOfCompartments&gt;</code> */
 , AllowedAttributesOnListOfSpecies      = 20226 /*!< Invalid attribute on <code>&lt;listOfSpecies&gt;</code> */
 , AllowedAttributesOnListOfParams       = 20227 /*!< Invalid attribute on <code>&lt;listOfParameters&gt;</code> */
 , AllowedAttributesOnListOfInitAssign   = 20228 /*!< Invalid attribute on <code>&lt;listOfInitialAssignments&gt;</code> */
 , AllowedAttributesOnListOfRules        = 20229 /*!< Invalid attribute on <code>&lt;listOfRules&gt;</code> */
 , AllowedAttributesOnListOfConstraints  = 20230 /*!< Invalid attribute on <code>&lt;listOfConstraints&gt;</code> */
 , AllowedAttributesOnListOfReactions    = 20231 /*!< Invalid attribute on <code>&lt;listOfReactions&gt;</code> */
 , AllowedAttributesOnListOfEvents       = 20232 /*!< Invalid attribute on <code>&lt;listOfEvents&gt;</code> */
 , FunctionDefMathNotLambda              = 20301 /*!< Invalid expression in function definition */
 , InvalidApplyCiInLambda                = 20302 /*!< Invalid forward reference in <code>&lt;apply&gt;</code><code>&lt;ci&gt;</code>...<code>&lt;/ci&gt;</code><code>&lt;/apply&gt;</code> value */
 , RecursiveFunctionDefinition           = 20303 /*!< Recursive function definition */
 , InvalidCiInLambda                     = 20304 /*!< Unknown <code>&lt;ci&gt;</code> reference in <code>&lt;lambda&gt;</code> */
 , InvalidFunctionDefReturnType          = 20305 /*!< Function return type must be either numerical or boolean */
 , OneMathElementPerFunc                 = 20306 /*!< FunctionDefinition must contain one math element */
 , AllowedAttributesOnFunc               = 20307 /*!< FunctionDefinition must have id and optionally metaid and sboTerm */
 , InvalidUnitDefId                      = 20401 /*!< Invalid <code>id</code> value for unit definition */
 , InvalidSubstanceRedefinition          = 20402 /*!< Invalid redefinition of <code>substance</code> */
 , InvalidLengthRedefinition             = 20403 /*!< Invalid redefinition of <code>length</code> */
 , InvalidAreaRedefinition               = 20404 /*!< Invalid redefinition of <code>area</code> */
 , InvalidTimeRedefinition               = 20405 /*!< Invalid redefinition of <code>time</code> */
 , InvalidVolumeRedefinition             = 20406 /*!< Invalid redefinition of <code>volume</code> */
 , VolumeLitreDefExponentNotOne          = 20407 /*!< Must use <code>exponent</code>=<code>1</code> when defining <code>volume</code> in terms of litres */
 , VolumeMetreDefExponentNot3            = 20408 /*!< Must use <code>exponent</code>=<code>3</code> when defining <code>volume</code> in terms of metres */
 , EmptyListOfUnits                      = 20409 /*!< Empty list of units not permitted */
 , InvalidUnitKind                       = 20410 /*!< Invalid value of <code>kind</code> in unit definition */
 , OffsetNoLongerValid                   = 20411 /*!< <code>offset</code> not supported in this Level+Version of SBML */
 , CelsiusNoLongerValid                  = 20412 /*!< <code>Celsius</code> not defined in this Level+Version of SBML */
 , EmptyUnitListElement                  = 20413 /*!< ListOfUnits must not be empty */
 , OneListOfUnitsPerUnitDef              = 20414 /*!< Only one ListOfUnits element on UnitDefinition */
 , OnlyUnitsInListOfUnits                = 20415 /*!< Only Units allowed in ListOfUnits */
 , AllowedAttributesOnUnitDefinition     = 20419 /*!< Invalid attribute on <code>&lt;unitDefinition&gt;</code> */
 , AllowedAttributesOnListOfUnits        = 20420 /*!< Invalid attribute on <code>&lt;listOfUnits&gt;</code> */
 , AllowedAttributesOnUnit               = 20421 /*!< Invalid attribute on <code>&lt;unit&gt;</code> */
 , ZeroDimensionalCompartmentSize        = 20501 /*!< Use of <code>size</code> is invalid for a zero-dimensional compartment */
 , ZeroDimensionalCompartmentUnits       = 20502 /*!< Use of <code>units</code> is invalid for a zero-dimensional compartment */
 , ZeroDimensionalCompartmentConst       = 20503 /*!< Zero-dimensional compartments cannot be non-constant */
 , UndefinedOutsideCompartment           = 20504 /*!< Undefined compartment used as <code>outside</code> value */
 , RecursiveCompartmentContainment       = 20505 /*!< Recursive nesting of compartments via <code>outside</code> */
 , ZeroDCompartmentContainment           = 20506 /*!< Invalid nesting of zero-dimensional compartments */
 , Invalid1DCompartmentUnits             = 20507 /*!< Invalid value of <code>units</code> for a one-dimensional compartment */
 , Invalid2DCompartmentUnits             = 20508 /*!< Invalid value of <code>units</code> for a two-dimensional compartment */
 , Invalid3DCompartmentUnits             = 20509 /*!< Invalid value of <code>units</code> for a three-dimensional compartment */
 , InvalidCompartmentTypeRef             = 20510 /*!< Invalid <code>compartmentType</code> reference */
 , OneDimensionalCompartmentUnits        = 20511 /*!< No units for 1D Compartment */
 , TwoDimensionalCompartmentUnits        = 20512 /*!< No units for 2D Compartment */
 , ThreeDimensionalCompartmentUnits      = 20513 /*!< No units for 3D Compartment */
 , AllowedAttributesOnCompartment        = 20517 /*!< Invalid attribute on <compartment> */
 , NoUnitsOnCompartment                  = 20518 /*!< No units for <compartment> */
 , InvalidSpeciesCompartmentRef          = 20601 /*!< Invalid <code>compartment</code> reference */
 , HasOnlySubsNoSpatialUnits             = 20602 /*!< No <code>spatialSizeUnits</code> permitted if <code>hasOnlySubstanceUnits</code>=<code>true</code> */
 , NoSpatialUnitsInZeroD                 = 20603 /*!< No <code>spatialSizeUnits</code> permitted if compartment is zero-dimensional */
 , NoConcentrationInZeroD                = 20604 /*!< No <code>initialConcentration</code> permitted if compartment is zero-dimensional */
 , SpatialUnitsInOneD                    = 20605 /*!< Invalid value of <code>spatialSizeUnits</code> for a one-dimensional compartment */
 , SpatialUnitsInTwoD                    = 20606 /*!< Invalid value of <code>spatialSizeUnits</code> for a two-dimensional compartment */
 , SpatialUnitsInThreeD                  = 20607 /*!< Invalid value of <code>spatialSizeUnits</code> for a three-dimensional compartment */
 , InvalidSpeciesSusbstanceUnits         = 20608 /*!< Invalid value of <code>units</code> */
 , BothAmountAndConcentrationSet         = 20609 /*!< Cannot set both <code>initialConcentration</code> and <code>initialAmount</code> */
 , NonBoundarySpeciesAssignedAndUsed     = 20610 /*!< Cannot use non-boundary species in both reactions and rules simultaneously */
 , NonConstantSpeciesUsed                = 20611 /*!< Cannot use non-boundary, constant species as reactant or product */
 , InvalidSpeciesTypeRef                 = 20612 /*!< Invalid <code>speciesType</code> reference */
 , MultSpeciesSameTypeInCompartment      = 20613 /*!< Cannot have multiple species of the same type in the same compartment */
 , MissingSpeciesCompartment             = 20614 /*!< Missing <code>compartment</code> value for species */
 , SpatialSizeUnitsRemoved               = 20615 /*!< Attribute <code>spatialSizeUnits</code> not supported in this Level+Version of SBML */
 , SubstanceUnitsOnSpecies               = 20616 /*!< No substance units for Species */
 , ConversionFactorOnSpecies             = 20617 /*!< Invalid conversionFactor attribute */
 , AllowedAttributesOnSpecies            = 20623 /*!< Invalid attribute on <species&gt;</code> */
 , InvalidParameterUnits                 = 20701 /*!< Invalid value for <code>units</code> in parameter definition */
 , ParameterUnits                        = 20702 /*!< No units for parameter */
 , ConversionFactorMustConstant          = 20705 /*!< ConversionFactor must be constant parameter */
 , AllowedAttributesOnParameter          = 20706 /*!< Invalid attribute on parameter */
 , InvalidInitAssignSymbol               = 20801 /*!< Invalid <code>symbol</code> reference in initial assignment */
 , MultipleInitAssignments               = 20802 /*!< Multiple initial assignments for the same <code>symbol</code> value */
 , InitAssignmentAndRuleForSameId        = 20803 /*!< Cannot set a value with both initial assignments and assignment rules simultaneously */
 , OneMathElementPerInitialAssign        = 20804 /*!< InitialAssignment must contain one math element */
 , AllowedAttributesOnInitialAssign      = 20805 /*!< Invalid attribute on initialAssignment */
 , InvalidAssignRuleVariable             = 20901 /*!< Invalid <code>variable</code> reference in assignment rule */
 , InvalidRateRuleVariable               = 20902 /*!< Invalid <code>variable</code> reference in rate rule */
 , AssignmentToConstantEntity            = 20903 /*!< Cannot reassign a constant in an assignment rule */
 , RateRuleForConstantEntity             = 20904 /*!< Cannot reassign a constant in a rate rule */
 , RepeatedRule10304                     = 20905 /*!<  */
 , CircularRuleDependency                = 20906 /*!< Circular dependency involving rules and reactions */
 , OneMathElementPerRule                 = 20907 /*!< Rule must contain one math element */
 , AllowedAttributesOnAssignRule         = 20908 /*!< Invalid attribute on <assignmentRule> */
 , AllowedAttributesOnRateRule           = 20909 /*!< Invalid attribute on <rateRule> */
 , AllowedAttributesOnAlgRule            = 20910 /*!< Invalid attribute on <algebraicRule> */
 , ConstraintMathNotBoolean              = 21001 /*!< Non-boolean math expression in constraint definition */
 , IncorrectOrderInConstraint            = 21002 /*!< Incorrect order of elements in constraint definition */
 , ConstraintNotInXHTMLNamespace         = 21003 /*!< Constraint message is not in XHTML XML namespace */
 , ConstraintContainsXMLDecl             = 21004 /*!< XML declarations not permitted in constraint messages */
 , ConstraintContainsDOCTYPE             = 21005 /*!< XML <code>DOCTYPE</code> not permitted in constraint messages */
 , InvalidConstraintContent              = 21006 /*!< Invalid content for constraint message */
 , OneMathElementPerConstraint           = 21007 /*!< Only one math element on <constraint> */
 , OneMessageElementPerConstraint        = 21008 /*!< Only one message element on <constraint> */
 , AllowedAttributesOnConstraint         = 21009 /*!< Invalid attribute on <constraint> */
 , NoReactantsOrProducts                 = 21101 /*!< Cannot have a reaction with neither reactants nor products */
 , IncorrectOrderInReaction              = 21102 /*!< Incorrect ordering of components in reaction definition */
 , EmptyListInReaction                   = 21103 /*!< Reaction components, if present, cannot be empty */
 , InvalidReactantsProductsList          = 21104 /*!< Invalid element in list of reactants or products */
 , InvalidModifiersList                  = 21105 /*!< Invalid element in list of modifiers */
 , OneSubElementPerReaction              = 21106 /*!< Only one of subelement on <reaction> */
 , CompartmentOnReaction                 = 21107 /*!< Invalid compartment attribute on <reaction> */
 , AllowedAttributesOnReaction           = 21110 /*!< Invalid attribute on <reaction> */
 , InvalidSpeciesReference               = 21111 /*!< Invalid <code>species</code> value in species reference */
 , RepeatedRule20611                     = 21112 /*!<  */
 , BothStoichiometryAndMath              = 21113 /*!< Cannot use both <code>stoichiometry</code> and <code>&lt;stoichiometryMath&gt;</code> simultaneously */
 , AllowedAttributesOnSpeciesReference   = 21116 /*!< Invalid attribute on <speciesReference> */
 , AllowedAttributesOnModifier           = 21117 /*!< Invalid attribute on <modifierSpeciesReference> */
 , UndeclaredSpeciesRef                  = 21121 /*!< Undeclared species referenced in kinetic law formula */
 , IncorrectOrderInKineticLaw            = 21122 /*!< Incorrect ordering of components in kinetic law definition */
 , EmptyListInKineticLaw                 = 21123 /*!< The list of parameters component, if present, cannot be empty */
 , NonConstantLocalParameter             = 21124 /*!< Parameters local to a kinetic law must have <code>constant</code>=<code>true</code> */
 , SubsUnitsNoLongerValid                = 21125 /*!< <code>substanceUnits</code> not supported in this Level+Version of SBML */
 , TimeUnitsNoLongerValid                = 21126 /*!< <code>timeUnits</code> not supported in this Level+Version of SBML */
 , OneListOfPerKineticLaw                = 21127 /*!< Only one listOfLocalParameters permitted on <kineticLaw> */
 , OnlyLocalParamsInListOfLocalParams    = 21128 /*!< Only LocalParameters allowed in ListOfLocalParameters */
 , AllowedAttributesOnListOfLocalParam   = 21129 /*!< Invalid attribute on <code>&lt;listOfLocalParameters&gt;</code> */
 , OneMathPerKineticLaw                  = 21130 /*!< Only one math element on <kineticLaw> */
 , UndeclaredSpeciesInStoichMath         = 21131 /*!< Undeclared species referenced in <code>&lt;stoichiometryMath&gt;</code> formula */
 , AllowedAttributesOnKineticLaw         = 21132 /*!< Invalid attribute on <kineticLaw> */
 , AllowedAttributesOnListOfSpeciesRef   = 21150 /*!< Invalid attribute on <code>&lt;listOfSpeciesReferences&gt;</code> */
 , AllowedAttributesOnListOfMods         = 21151 /*!< Invalid attribute on <code>&lt;listofModifiers&gt;</code> */
 , AllowedAttributesOnLocalParameter     = 21172 /*!< Invalid attribute on <localParameter> */
 , MissingTriggerInEvent                 = 21201 /*!< Missing trigger in event definition */
 , TriggerMathNotBoolean                 = 21202 /*!< Non-boolean math expression in trigger definition */
 , MissingEventAssignment                = 21203 /*!< Missing event assignment in event definition */
 , TimeUnitsEvent                        = 21204 /*!< Units of <code>timeUnits</code> are not time units */
 , IncorrectOrderInEvent                 = 21205 /*!< Incorrect ordering of components in event definition */
 , ValuesFromTriggerTimeNeedDelay        = 21206 /*!< <code>useValuesFromTriggerTime</code>=<code>false</code>, but no delay defined in event */
 , DelayNeedsValuesFromTriggerTime       = 21207 /*!< Delay requires useValuesFromTriggerTime */
 , OneMathPerTrigger                     = 21209 /*!< Trigger must have one math element */
 , OneMathPerDelay                       = 21210 /*!< Delay must have one math element */
 , InvalidEventAssignmentVariable        = 21211 /*!< Invalid value for <code>variable</code> in event assignment */
 , EventAssignmentForConstantEntity      = 21212 /*!< Cannot assign to a constant component in an event assignment */
 , OneMathPerEventAssignment             = 21213 /*!< EventAssignment must have one math element */
 , AllowedAttributesOnEventAssignment    = 21214 /*!< Invalid attribute on <eventAssignment> */
 , OnlyOneDelayPerEvent                  = 21221 /*!< Event can only have one <delay> */
 , OneListOfEventAssignmentsPerEvent     = 21222 /*!< Event can only have one <code>&lt;listOfEventAssignments&gt;</code> */
 , OnlyEventAssignInListOfEventAssign    = 21223 /*!< <code>&lt;listOfEventAssignments&gt;</code> can only have <eventAssignment> */
 , AllowedAttributesOnListOfEventAssign  = 21224 /*!< Invalid attribute on <code>&lt;listOfEventAssignments&gt;</code> */
 , AllowedAttributesOnEvent              = 21225 /*!< Invalid attribute on <event> */
 , AllowedAttributesOnTrigger            = 21226 /*!< Invalid attribute on <trigger> */
 , AllowedAttributesOnDelay              = 21227 /*!< Invalid attribute on <delay> */
 , PersistentNotBoolean                  = 21228 /*!< Persistent attribute on <trigger> must be boolean */
 , InitialValueNotBoolean                = 21229 /*!< InitialValue attribute on <trigger> must be boolean */
 , OnlyOnePriorityPerEvent               = 21230 /*!< Event can only have one <priority> */
 , OneMathPerPriority                    = 21231 /*!< Priority must have one math element */
 , AllowedAttributesOnPriority           = 21232 /*!< Invalid attribute on <priority> */


 , GeneralWarningNotSpecified            = 29999 /*!< Unknown error */

  /* ModelingPractice contraints */

 , CompartmentShouldHaveSize             = 80501 /*!< It's best to define a size for every compartment in a model */
 , ParameterShouldHaveUnits              = 80701 /*!< It's best to declare units for every parameter in a model */
 , LocalParameterShadowsId               = 81121 /*!< Local parameters defined in a kinetic law shadow global parameters */
    
  /* Lower bound for additional error codes returned by libSBML but not
   * defined in SBML specifications. */

 , LibSBMLAdditionalCodesLowerBound      = 90000 /*!< Lower bound of libSBML-specific codes */

 , CannotConvertToL1V1                   = 90001 /*!< Cannot convert to SBML Level&nbsp;1 Version&nbsp;1 */

  /* L1Compatability */

 , NoEventsInL1                          = 91001 /*!< SBML Level&nbsp;1 does not support events */
 , NoFunctionDefinitionsInL1             = 91002 /*!< SBML Level&nbsp;1 does not support function definitions */
 , NoConstraintsInL1                     = 91003 /*!< SBML Level&nbsp;1 does not support constraints */
 , NoInitialAssignmentsInL1              = 91004 /*!< SBML Level&nbsp;1 does not support initial assignments */
 , NoSpeciesTypesInL1                    = 91005 /*!< SBML Level&nbsp;1 does not support species types */
 , NoCompartmentTypeInL1                 = 91006 /*!< SBML Level&nbsp;1 does not support compartment types */
 , NoNon3DComparmentsInL1                = 91007 /*!< SBML Level&nbsp;1 only supports three-dimensional compartments */
 , NoFancyStoichiometryMathInL1          = 91008 /*!< SBML Level&nbsp;1 does not support non-integer nor non-rational stoichiometry formulas */
 , NoNonIntegerStoichiometryInL1         = 91009 /*!< SBML Level&nbsp;1 does not support non-integer <code>stoichiometry</code> attribute values */
 , NoUnitMultipliersOrOffsetsInL1        = 91010 /*!< SBML Level&nbsp;1 does not support multipliers or offsets in unit definitions */
 , SpeciesCompartmentRequiredInL1        = 91011 /*!< In SBML Level&nbsp;1, a value for <code>compartment</code> is mandatory in species definitions */
 , NoSpeciesSpatialSizeUnitsInL1         = 91012 /*!< SBML Level&nbsp;1 does not support species <code>spatialSizeUnits</code> settings */
 , NoSBOTermsInL1                        = 91013 /*!< SBML Level&nbsp;1 does not support the <code>sboTerm</code> attribute */
 , StrictUnitsRequiredInL1               = 91014 /*!< SBML Level&nbsp;1 requires strict unit consistency */
 , ConversionFactorNotInL1               = 91015 /*!< SBML Level&nbsp;1 does not support the <code>conversionFactor</code> attribute */
 , CompartmentNotOnL1Reaction            = 91016 /*!< SBML Level&nbsp;1 does not support the <code>compartment</code> attribute on a <Reaction> */
 , ExtentUnitsNotSubstance               = 91017 /*!< Conversion to SBML Level&nbsp;1 requires that <code>extent</code> units be a variant of substance */

  /* L2v1 compatability */

 , NoConstraintsInL2v1                   = 92001 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support constraints */
 , NoInitialAssignmentsInL2v1            = 92002 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support initial assignments */
 , NoSpeciesTypeInL2v1                   = 92003 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support species types */
 , NoCompartmentTypeInL2v1               = 92004 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support compartment types */
 , NoSBOTermsInL2v1                      = 92005 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support the <code>sboTerm</code> attribute */
 , NoIdOnSpeciesReferenceInL2v1          = 92006 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support the <code>id</code> attribute on species references */
 , NoDelayedEventAssignmentInL2v1        = 92007 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support the <code>useValuesFromTriggerTime</code> attribute */
 , StrictUnitsRequiredInL2v1             = 92008 /*!< SBML Level&nbsp;2 Version&nbsp;1 requires strict unit consistency */
 , IntegerSpatialDimensions              = 92009 /*!< SBML Level&nbsp;2 Version&nbsp;1 requires spatial dimensions of 0 - 3 */
 , StoichiometryMathNotYetSupported      = 92010 /*!< Conversion to StoichiometryMath not yet supported */
 , PrioirtyLostFromL3                    = 92011 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support priority */
 , NonPersistentNotSupported             = 92012 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support the <code>persistent</code> attribute */
 , InitialValueFalseEventNotSupported    = 92013 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support an <code>initialValue</code> of <code>false</code> for a <Trigger> */

 /*!< SBML Level&nbsp;2 Version&nbsp;1 requires strict unit consistency */

  /* L2v2 compatability */

 , SBOTermNotUniversalInL2v2             = 93001 /*!< The <code>sboTerm</code> attribute is invalid for this component in Level&nbsp;2 Version&nbsp;2 */
 , NoUnitOffsetInL2v2                    = 93002 /*!< The unit <code>offset</code> attribute is invalid in this Level+Version of SBML */
 , NoKineticLawTimeUnitsInL2v2           = 93003 /*!< The <code>timeUnits</code> attribute is invalid in this Level+Version of SBML */
 , NoKineticLawSubstanceUnitsInL2v2      = 93004 /*!< The <code>substanceUnits</code> attribute is invalid in this Level+Version of SBML */
 , NoDelayedEventAssignmentInL2v2        = 93005 /*!< Attribute <code>useValuesFromTriggerTime</code> not supported in this Level+Version of SBML */
 , ModelSBOBranchChangedBeyondL2v2       = 93006 /*!< The allowable <code>sboTerm</code> values for model differ for this SBML Level+Version */
 , StrictUnitsRequiredInL2v2             = 93007 /*!< SBML Level&nbsp;2 Version&nbsp;2 requires strict unit consistency */
 , StrictSBORequiredInL2v2               = 93008 /*!< SBML Level&nbsp;2 Version&nbsp;2 requires strict sbo consistency */
 , DuplicateAnnotationInvalidInL2v2      = 93009 /*!< Duplicate top level annotations invalid for this SBML Level+Version */

  /* L2v3 compatability  */

 , NoUnitOffsetInL2v3                    = 94001 /*!< Attribute <code>offset</code> not supported in this Level+Version of SBML */
 , NoKineticLawTimeUnitsInL2v3           = 94002 /*!< Attribute <code>timeUnits</code> not supported in this Level+Version of SBML */
 , NoKineticLawSubstanceUnitsInL2v3      = 94003 /*!< Attribute <code>substanceUnits</code> not supported in this Level+Version of SBML */
 , NoSpeciesSpatialSizeUnitsInL2v3       = 94004 /*!< Attribute <code>spatialSizeUnits</code> not supported in this Level+Version of SBML */
 , NoEventTimeUnitsInL2v3                = 94005 /*!< Attribute <code>timeUnits</code> not supported in this Level+Version of SBML */
 , NoDelayedEventAssignmentInL2v3        = 94006 /*!< Attribute <code>useValuesFromTriggerTime</code> not supported in this Level+Version of SBML */
 , ModelSBOBranchChangedBeyondL2v3       = 94007 /*!< The allowable <code>sboTerm</code> values for model differ for this SBML Level+Version */
 , StrictUnitsRequiredInL2v3             = 94008 /*!< SBML Level&nbsp;2 Version&nbsp;3 requires strict unit consistency */
 , StrictSBORequiredInL2v3               = 94009 /*!< SBML Level&nbsp;2 Version&nbsp;3 requires strict sbo consistency */
 , DuplicateAnnotationInvalidInL2v3      = 94010 /*!< Duplicate top level annotations invalid for this SBML Level+Version */

  /* L2v4 compatability  */

 , NoUnitOffsetInL2v4                    = 95001 /*!< The unit <code>offset</code> attribute is invalid in this Level+Version of SBML */
 , NoKineticLawTimeUnitsInL2v4           = 95002 /*!< The <code>timeUnits</code> attribute is invalid in this Level+Version of SBML */
 , NoKineticLawSubstanceUnitsInL2v4      = 95003 /*!< The <code>substanceUnits</code> attribute is invalid in this Level+Version of SBML */
 , NoSpeciesSpatialSizeUnitsInL2v4       = 95004 /*!< The <code>spatialSizeUnits</code> attribute is invalid in this Level+Version of SBML */
 , NoEventTimeUnitsInL2v4                = 95005 /*!< The <code>timeUnits</code> attribute is invalid in this Level+Version of SBML */
 , ModelSBOBranchChangedInL2v4           = 95006 /*!< The allowable <code>sboTerm</code> values for model differ for this SBML Level+Version */
 , DuplicateAnnotationInvalidInL2v4      = 95007 /*!< Duplicate top level annotations invalid for this SBML Level+Version */

  /* L3v1 compatability */

 , NoSpeciesTypeInL3v1                   = 96001 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support species types */
 , NoCompartmentTypeInL3v1               = 96002 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support compartment types */
 , NoUnitOffsetInL3v1                    = 96003 /*!< The unit <code>offset</code> attribute is invalid in this Level+Version of SBML */
 , NoKineticLawTimeUnitsInL3v1           = 96004 /*!< The <code>timeUnits</code> attribute is invalid in this Level+Version of SBML */
 , NoKineticLawSubstanceUnitsInL3v1      = 96005 /*!< The <code>substanceUnits</code> attribute is invalid in this Level+Version of SBML */
 , NoSpeciesSpatialSizeUnitsInL3v1       = 96006 /*!< The <code>spatialSizeUnits</code> attribute is invalid in this Level+Version of SBML */
 , NoEventTimeUnitsInL3v1                = 96007 /*!< The <code>timeUnits</code> attribute is invalid in this Level+Version of SBML */
 , ModelSBOBranchChangedInL3v1           = 96008 /*!< The allowable <code>sboTerm</code> values for model differ for this SBML Level+Version */
 , DuplicateAnnotationInvalidInL3v1      = 96009 /*!< Duplicate top level annotations invalid for this SBML Level+Version */
 , NoCompartmentOutsideInL3v1            = 96010 /*!< The unit <code>offset</code> attribute is invalid in this Level+Version of SBML */
 , NoStoichiometryMathInL3v1             = 96011 /*!< SBML Level&nbsp;3 Version&nbsp;1 does not support stoichiometryMath */

 /* These are errors checked by libSBML that were never
   * published in a spec. */

 , InvalidSBMLLevelVersion               = 99101 /*!< Invalid SBML Level and Version */
 , AnnotationNotesNotAllowedLevel1       = 99104 /*!< Annotation on <code>&lt;sbml&gt;</code> not permitted in SBML Level&nbsp;1 */
 , InvalidRuleOrdering                   = 99106 /*!< Invalid ordering of rules */
 , RequiredPackagePresent                = 99107 /*!< Model requires an L3 package */

 , SubsUnitsAllowedInKL                  = 99127 /*!< Disallowed value for attribute <code>substanceUnits</code> */
 , TimeUnitsAllowedInKL                  = 99128 /*!< Disallowed value for attribute <code>timeUnits</code> */
 , FormulaInLevel1KL                     = 99129 /*!< Only predefined functions are permitted in SBML Level&nbsp;1 formulas */

 , TimeUnitsRemoved                      = 99206  /*!< The <code>timeUnits</code> attribute is invalid in this Level+Version of SBML */

 , BadMathML                             = 99219 /*!< Invalid MathML expression */
 , FailedMathMLReadOfDouble              = 99220 /*!< Failed to read floating-point number */
 , FailedMathMLReadOfInteger             = 99221 /*!< Failed to read an integer */
 , FailedMathMLReadOfExponential         = 99222 /*!< Failed to read an exponential expression */
 , FailedMathMLReadOfRational            = 99223 /*!< Failed to read a rational expression */
 , BadMathMLNodeType                     = 99224 /*!< Invalid MathML element */

 , NoTimeSymbolInFunctionDef             = 99301 /*!< <code>&lt;csymbol&gt;</code> for <code>time</code> used within the <code>&lt;math&gt;</code> of a function definition */

 , NoBodyInFunctionDef                   = 99302 /*!< There must be a body within the <math> of a function definition */

  /* These are internal errors that reverts to 10501. */

  /** @cond doxygen-libsbml-internal */
 , InconsistentArgUnitsWarnings          = 99502 /*!< SBML L2v3 validation rule #10501 */
 , InconsistentPowerUnitsWarnings        = 99503 /*!< SBML L2v3 validation rule #10501 */
 , InconsistentExponUnitsWarnings        = 99504 /*!< SBML L2v3 validation rule #10501 */
  /** @endcond */

 , UndeclaredUnits                       = 99505 /*!< Undeclared units */
 , UnrecognisedSBOTerm                   = 99701 /*!< Unrecognized <code>sboTerm</code> value */
 , ObseleteSBOTerm                       = 99702 /*!< Obsolete <code>sboTerm</code> value */

  /* Internal consistency checks */

 , IncorrectCompartmentSpatialDimensions = 99901 /*!< In SBML Level&nbsp;1, only three-dimensional compartments are permitted */
 , CompartmentTypeNotValidAttribute      = 99902 /*!< Compartment types not supported in this Level+Version of SBML */
 , ConstantNotValidAttribute             = 99903 /*!< Attribute <code>constant</code> not supported on this component in SBML Level&nbsp;1 */
 , MetaIdNotValidAttribute               = 99904 /*!< Attribute <code>metaid</code> not supported in SBML Level&nbsp;1 */
 , SBOTermNotValidAttributeBeforeL2V3    = 99905 /*!< <code>sboTerm</code> not available on this component before SBML Level&nbsp;2 Version&nbsp;3 */
 , InvalidL1CompartmentUnits             = 99906 /*!< Invalid units for a compartment in SBML Level&nbsp;1 */
 , L1V1CompartmentVolumeReqd             = 99907 /*!< Compartment volume must be specified */
 , CompartmentTypeNotValidComponent      = 99908 /*!< Compartment types not supported in this Level+Version of SBML */
 , ConstraintNotValidComponent           = 99909 /*!< Constraints not supported in this Level+Version of SBML */
 , EventNotValidComponent                = 99910 /*!< Events not supported in this Level+Version of SBML */
 , SBOTermNotValidAttributeBeforeL2V2    = 99911 /*!< The <code>sboTerm</code> attribute is invalid for this component before Level&nbsp;2 Version&nbsp;2 */
 , FuncDefNotValidComponent              = 99912 /*!< Function definitions are not supported in this Level+Version of SBML */
 , InitialAssignNotValidComponent        = 99913 /*!< Initial assignments are not supported in this Level+Version of SBML */
 , VariableNotValidAttribute             = 99914 /*!< Attribute <code>variable</code> not valid */
 , UnitsNotValidAttribute                = 99915 /*!< Attribute <code>units</code> not valid */
 , ConstantSpeciesNotValidAttribute      = 99916 /*!< Attribute <code>constant</code> on species not supported in SBML Level&nbsp;1 */
 , SpatialSizeUnitsNotValidAttribute     = 99917 /*!< Attribute <code>spatialSizeUnits</code> on species not supported in SBML Level&nbsp;1 */
 , SpeciesTypeNotValidAttribute          = 99918 /*!< Attribute <code>speciesType</code> on species not supported in SBML Level&nbsp;1 */
 , HasOnlySubsUnitsNotValidAttribute     = 99919 /*!< Attribute <code>hasOnlySubstanceUnits</code> on species not supported in SBML Level&nbsp;1 */
 , IdNotValidAttribute                   = 99920 /*!< Attribute <code>id</code> on species references not supported in SBML Level&nbsp;1 */
 , NameNotValidAttribute                 = 99921 /*!< Attribute <code>name</code> on species references not supported in SBML Level&nbsp;1 */
 , SpeciesTypeNotValidComponent          = 99922 /*!< Species types not supported in SBML Level&nbsp;1 */
 , StoichiometryMathNotValidComponent    = 99923 /*!< <code>&lt;stoichiometryMath&gt;</code> not supported in SBML Level&nbsp;1 */
 , MultiplierNotValidAttribute           = 99924 /*!< Attribute <code>multiplier</code> on units not supported in SBML Level&nbsp;1 */
 , OffsetNotValidAttribute               = 99925 /*!< Attribute <code>offset</code> on units only available in SBML Level&nbsp;2 Version&nbsp;1 */

 , InvalidTargetLevelVersion             = 99997 /*!< The target Level/Version&nbspdoes not exist */
 , L3NotSupported                        = 99998 /*!< SBML Level&nbsp;3 is not yet supported */

 /* Bounds */

  , SBMLCodesUpperBound                  = 99999 /*!< 99999, the upper bound 
                                                  * all libSBML codes.  
                                                  * Application-specific codes
                                                  * should begin at 100000. */
} SBMLErrorCode_t;


/**
 * Category codes for SBMLError diagnostics.
 *
 * Note that these are distinct from XMLError's category codes.  User
 * programs receiving an SBMLError object can use this distinction to
 * check whether the error represents a low-level XML problem or an
 * SBML problem.
 *
 * @see #XMLErrorCategory_t
 */
typedef enum 
{
    LIBSBML_CAT_SBML = (LIBSBML_CAT_XML + 1)
    /*!< General SBML error  not falling into another category below. */

  , LIBSBML_CAT_SBML_L1_COMPAT
    /*!< Category of errors that can only occur during attempted
     * translation from one Level/Version of SBML to another.  This
     * particular category applies to errors encountered while trying to
     * convert a model from SBML Level&nbsp;2 to SBML Level&nbsp;1. */

  , LIBSBML_CAT_SBML_L2V1_COMPAT
    /*!< Category of errors that can only occur during attempted
     * translation from one Level/Version of SBML to another.  This
     * particular category applies to errors encountered while trying to
     * convert a model to SBML Level&nbsp;2 Version&nbsp;1. */

  , LIBSBML_CAT_SBML_L2V2_COMPAT
    /*!< Category of errors that can only occur during attempted
     * translation from one Level/Version of SBML to another.  This
     * particular category applies to errors encountered while trying to
     * convert a model to SBML Level&nbsp;2 Version&nbsp;2. */

  , LIBSBML_CAT_GENERAL_CONSISTENCY
    /*!< Category of errors that can occur while validating general SBML
     * constructs.  With respect to the SBML specification, these concern
     * failures in applying the validation rules numbered 2xxxx in the
     * Level&nbsp;2 Versions&nbsp;2 and&nbsp;3 specifications. */

  , LIBSBML_CAT_IDENTIFIER_CONSISTENCY
    /*!< Category of errors that can occur while validating symbol
     * identifiers in a model.  With respect to the SBML specification,
     * these concern failures in applying the validation rules numbered
     * 103xx in the Level&nbsp;2 Versions&nbsp;2 and&nbsp;3 specifications. */

  , LIBSBML_CAT_UNITS_CONSISTENCY
    /*!< Category of errors that can occur while validating the units of
     * measurement on quantities in a model.  With respect to the SBML
     * specification, these concern failures in applying the validation
     * rules numbered 105xx in the Level&nbsp;2 Versions&nbsp;2 and&nbsp;3
     * specifications. */

  , LIBSBML_CAT_MATHML_CONSISTENCY
    /*!< Category of errors that can occur while validating MathML formulas
     * in a model.  With respect to the SBML specification, these concern
     * failures in applying the validation rules numbered 102xx in the
     * Level&nbsp;2 Versions&nbsp;2 and&nbsp;3 specifications. */

  , LIBSBML_CAT_SBO_CONSISTENCY
    /*!< Category of errors that can occur while validating SBO identifiers
     * in a model.  With respect to the SBML specification, these concern
     * failures in applying the validation rules numbered 107xx in the
     * Level&nbsp;2 Versions&nbsp;2 and&nbsp;3 specifications. */

  , LIBSBML_CAT_OVERDETERMINED_MODEL
    /*!< Error in the system of equations in the model: the system is
     * overdetermined, therefore violating a tenet of proper SBML.  With
     * respect to the SBML specification, this is validation rule #10601 in
     * the SBML Level&nbsp;2 Versions&nbsp;2 and&nbsp;3 specifications. */

  , LIBSBML_CAT_SBML_L2V3_COMPAT
    /*!< Category of errors that can only occur during attempted
     * translation from one Level/Version of SBML to another.  This
     * particular category applies to errors encountered while trying to
     * convert a model to SBML Level&nbsp;2 Version&nbsp;3. */

  , LIBSBML_CAT_MODELING_PRACTICE
    /*!< Category of warnings about recommended good practices involving
     * SBML and computational modeling.  (These are tests performed by
     * libSBML and do not have equivalent SBML validation rules.) */

  , LIBSBML_CAT_INTERNAL_CONSISTENCY
    /*!< Category of errors that can occur while validating libSBML's 
     * internal representation of SBML constructs. (These are tests 
     * performed by libSBML and do not have equivalent SBML validation 
     * rules.)  */

  , LIBSBML_CAT_SBML_L2V4_COMPAT
    /*!< Category of errors that can only occur during attempted
     * translation from one Level/Version of SBML to another.  This
     * particular category applies to errors encountered while trying to
     * convert a model to SBML Level&nbsp;2 Version&nbsp;4. */

  , LIBSBML_CAT_SBML_L3V1_COMPAT
    /*!< Category of errors that can only occur during attempted
     * translation from one Level/Version of SBML to another.  This
     * particular category applies to errors encountered while trying to
     * convert a model to SBML Level&nbsp;3 Version&nbsp;1. */

} SBMLErrorCategory_t;


/**
 * Severity codes for SBMLError diagnostics.
 *
 * These are distinct from XMLError's severity codes.  
 *
 * @see XMLErrorSeverity_t
 */
typedef enum
{
  /** @cond doxygen-libsbml-internal **/

  /* The following are used internally in SBMLErrorTable, but publicly,
   * we only report one of the 4 XMLError_Severity values.  Translation
   * of the codes is done in SBMLError.cpp.
   */

    LIBSBML_SEV_SCHEMA_ERROR    = (LIBSBML_SEV_FATAL + 1)
    /*!< The XML content does not conform to
     * the relevant version of the SBML XML 
     * Schema.  The content is not valid SBML. */

  , LIBSBML_SEV_GENERAL_WARNING
    /*!< The XML content is invalid for some
     * levels/versions of SBML, and while it
     * may be valid in others, it is something
     * that is best avoided anyway.  LibSBML
     * will issue warnings in those cases it
     * can recognize. */

  , LIBSBML_SEV_NOT_APPLICABLE
    /*!< This error code is only a placeholder
     * for errors that have relevance to some
     * versions of SBML but not others. */

  /** @endcond **/
} SBMLErrorSeverity_t;

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN SBMLError : public XMLError
{
public:

  /**
   * Creates a new SBMLError to report that something occurred during SBML
   * processing.
   *
   * When a libSBML operation on SBML content results in a warning, error
   * or other diagnostic, the issue is reported as an SBMLError object.
   * SBMLError objects have identification numbers to indicate the nature
   * of the exception.  @if clike These numbers are drawn from
   * the enumeration <a class="el"
   * href="#SBMLErrorCode_t">
   * SBMLErrorCode_t</a>.  @endif@if java These numbers are
   * defined as unsigned integer constants in the file
   * "libsbmlConstants.html".  See the <a class="el"
   * href="#SBMLErrorCode_t">top of this documentation page</a> for a table
   * listing the possible values and their meanings. @endif@if python These
   * numbers are defined as unsigned integer constants in the interface
   * class @link libsbml libsbml@endlink.  See the <a class="el"
   * href="#SBMLErrorCode_t">top of this documentation page</a> for a table
   * listing the possible values and their meanings. @endif The argument 
   * @p errorId to this constructor @em can be (but does not have to be) a
   * value from this @if clike enumeration. If it @em is a value
   * from <a class="el" href="#SBMLErrorCode_t">SBMLErrorCode_t</a>, the
   * SBMLError class assumes the error is a low-level system or SBML layer
   * error and <em>prepends</em> a built-in, predefined error message to
   * any string passed in the argument @p details to this constructor.  In
   * addition, all <a class="el"
   * href="#SBMLErrorCode_t">SBMLErrorCode_t</a> errors have associated
   * values for the @p severity and @p category codes, and these fields are
   * filled-in as well from the enumerations <a class="el"
   * href="#SBMLErrorSeverity_t">SBMLErrorSeverity_t</a> and <a class="el"
   * href="#SBMLErrorCategory_t">SBMLErrorCategory_t</a>,
   * respectively. @endif@if notcpp set of constants.  If it @em
   * is one of the predefined error identifiers, the SBMLError class
   * assumes the error is a low-level system or SBML layer error and
   * <em>prepends</em> a built-in, predefined error message to any string
   * passed in the argument @p details to this constructor.  In addition,
   * all the predefined error identifiers have associated values for the 
   * @p severity and @p category codes, and these fields are filled-in using
   * the libSBML defaults for each different error identifier. @endif
   *
   * If the error identifier @p errorId is a number greater than 99999, the
   * SBMLError class assumes the error was generated from another part of
   * the software and does not do additional filling in of values beyond
   * the default in the constructor itself.  This allows SBMLError to serve
   * as a base class for other errors, such as for user-defined validation
   * rules (see Validator).  Callers should fill in all the parameters with
   * suitable values if generating errors with codes greater than 99999 to
   * make maximum use of the SBMLError facilities.
   *
   * @if clike As mentioned above, there are two other
   * enumerations, <a class="el"
   * href="#SBMLErrorSeverity_t">SBMLErrorSeverity_t</a> and <a class="el"
   * href="#SBMLErrorCategory_t">SBMLErrorCategory_t</a>, used for indicating
   * the severity and category of error for the predefined SBMLError codes.
   * The values passed in @p severity and @p category override the defaults
   * assigned based on the error code.  If the value of @p errorId is a
   * value from <a class="el" href="#SBMLErrorCode_t">SBMLErrorCode_t</a>,
   * callers do not need to fill in @p severity and @p category.
   * Conversely, if @p errorId is not a value from <a class="el"
   * href="#SBMLErrorCode_t">SBMLErrorCode_t</a>, callers can use other
   * values (not just those from <a class="el"
   * href="#SBMLErrorSeverity_t">SBMLErrorSeverity_t</a> and <a class="el"
   * href="#SBMLErrorCategory_t">SBMLErrorCategory_t</a>, but their own
   * special values) for @p severity and 
   * @p category. @endif@if notcpp As mentioned above, 
   * there are additional constants defined for <a class="el"
   * href="#SBMLErrorSeverity_t">standard severity</a> and <a class="el"
   * href="#SBMLErrorCategory_t">standard category</a> codes, and every predefined 
   * error in libSBML has an associated value for severity and category taken
   * from these predefined sets.  These constants have symbol names
   * prefixed with <code>LIBSBML_SEV_</code> and <code>LIBSBML_CAT_</code>,
   * respectively.  If the value of @p errorId is one of the standard error
   * codes, callers do not need to fill in @p severity and @p category in a
   * call to this constructor.  Conversely, if @p errorId is not an existing
   * SBML-level error code, callers can use other values for @p severity and
   * @p category. @endif
   *
   * Please see the top of the documentation for SBMLError for a longer
   * discussion of the possible error codes, their meanings, and their
   * applicability to different combinations of Level+Version of SBML.
   *
   * @param errorId an unsigned int, the identification number of the error.
   *
   * @param level the SBML Level of the SBML model
   *
   * @param version the SBML Version within the Level of the SBML model
   * 
   * @param details a string containing additional details about the error.
   * If the error code in @p errorId is one that is recognized by SBMLError,
   * the given message is @em appended to a predefined message associated
   * with the given code.  If the error code is not recognized, the message
   * is stored as-is as the text of the error.
   * 
   * @param line an unsigned int, the line number at which the error occured.
   * 
   * @param column an unsigned int, the column number at which the error occured.
   * 
   * @param severity an integer indicating severity of the error.
   * 
   * @param category an integer indicating the category to which the error
   * belongs.
   *
   * @if notcpp @docnote @htmlinclude warn-default-args-in-docs.html @endif
   */
  SBMLError
  (
     const unsigned int errorId  = 0
   , const unsigned int level    = SBML_DEFAULT_LEVEL
   , const unsigned int version  = SBML_DEFAULT_VERSION
   , const std::string& details  = ""
   , const unsigned int line     = 0
   , const unsigned int column   = 0
   , const unsigned int severity = LIBSBML_SEV_ERROR
   , const unsigned int category = LIBSBML_CAT_SBML
  );


  /**
   * Copy constructor; creates a copy of this SBMLError.
   */
  SBMLError(const SBMLError& orig);


#ifndef SWIG

  /** @cond doxygen-libsbml-internal **/

  /**
   * clone function
   */
  virtual SBMLError* clone() const; 

  /**
   * Outputs this SBMLError to stream in the following format (and followed
   * by a newline):
   *
   *   line: (error id) message
   *
   * @param stream the output stream to write to.
   */
  virtual void print(std::ostream& stream) const;

  /** @endcond **/

#endif  /* !SWIG */

protected:
  /** @cond doxygen-libsbml-internal **/

  virtual std::string stringForSeverity(unsigned int code) const;
  virtual std::string stringForCategory(unsigned int code) const;

  /** @endcond **/
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif /* SBMLError_h */
