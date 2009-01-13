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
 * Copyright 2005-2009 California Institute of Technology.
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
 * @htmlinclude libsbml-not-sbml-warning.html
 *
 * When a libSBML operation on SBML content results in an error, or when
 * there is something wrong with the SBML content, the problems are
 * reported as SBMLError objects.  These are generally stored in an
 * SBMLErrorLog object; this log object, in turn, is kept in the
 * SBMLDocument object containing the SBML content.  Applications can
 * obtain the list of logged errors using SBMLDocument.getErrorLog() and
 * then use the methods provided by SBMLErrorLog to access individual
 * SBMLError objects.  (Note that despite the word "error" in the name,
 * SBMLError objects are used to represent not only "true" errors, but also
 * warnings and some informational diagnostics.  The name is a historical
 * hold-over from early versions of libSBML, in which the object really was
 * only used to report errors.)
 *
 * Each SBMLError object instance has an identification number that
 * identifies the nature of the problem.  This "error id" number will be up
 * to five digits long, and it will be listed in one of two enumerations,
 * @link XMLError::XMLErrorCode_t XMLErrorCode_t @endlink and <a class="el"
 * href="#SBMLErrorCode_t">SBMLErrorCode_t</a>.  The latter enumeration
 * contains all the SBML validation rule numbers listed in the appendices
 * of the SBML specification documents, as well as some additional
 * libSBML-specific error codes.  
 * 
 * The error codes are useful for software, but less so for humans.  So, for
 * human consumption, SBMLError also includes text messages that describe
 * the nature of a given problem.  The messages can be accessed using
 * getShortMessage() and getMessage().  The former provides a brief
 * one-line description of the issue, while getMessage() provides a more
 * detailed text, including (if appropriate) references to sections of the
 * SBML specifications where relevant topics are discussed.
 *
 * An SBMLError object also contains a @em category code, drawn from the
 * enumeration <a class="el"
 * href="#SBMLErrorCategory_t">SBMLErrorCategory_t</a>.  Categories are
 * used to partition errors into distinct conceptual groups.  This is
 * principally used by the libSBML validation system to group classes of
 * validation checks into groups.  For example, @c
 * LIBSBML_CAT_IDENTIFIER_CONSISTENCY is the category for tests that check
 * identifier consistency; @c LIBSBML_CAT_MATHML_CONSISTENCY is the
 * category for MathML consistency checking; and so on.  <a class="el"
 * href="#SBMLErrorCategory_t">SBMLErrorCategory_t</a> is an extension of
 * @link XMLError::XMLErrorCategory_t XMLErrorCategory_t @endlink.
 *
 * In addition, SBMLError also has a @em severity code, drawn from the
 * enumeration <a class="el"
 * href="#SBMLErrorSeverity_t">SBMLErrorSeverity_t</a>.  Severity levels
 * currently range from informational (@c LIBSBML_SEV_INFO) to fatal errors
 * (@c LIBSBML_SEV_FATAL).  They can be used by an application to evaluate
 * how serious a given problem is.  <a class="el"
 * href="#SBMLErrorSeverity_t">SBMLErrorSeverity_t</a> is an extension of
 * @link XMLError::XMLErrorSeverity_t XMLErrorSeverity_t @endlink.
 *
 * Finally, SBMLError records the line and column near where the problem
 * occurred in the SBML content.  We say "near", because a lot of factors
 * affect how accurate the line/column information ultimately is.  For
 * example, different XML parsers have different conventions for which line
 * and column number they report for a particular problem (which makes a
 * difference when a problem involves an opening XML tag on one line and a
 * closing tag on another line).  If your application communicates problems
 * to humans, we suggest is generally best to provide not only a
 * warning/error message (either from getShortMessage() or, preferrably,
 * getMessage(), but also the line and column numbers, to help them
 * determine the actual error.
 *
 * <h3><a class="anchor" name="SBMLErrorCode_t">SBMLErrorCode_t</a></h3>
 *
 * This is an enumeration of all SBML-level error, warning and
 * informational diagnostic codes.  Every SBMLError object has an error
 * code value that can be either a value from this enumeration, or a value
 * from the @link XMLError::XMLErrorCode_t XMLErrorCode_t @endlink
 * enumeration (see the documentation for XMLError).  The latter values
 * apply when the error or warning signifies a basic XML issue rather than
 * an SBML issue per se.  The values of SBMLErrorCode_t are distinguished
 * from those of @link XMLError::XMLErrorCode_t XMLErrorCode_t @endlink by
 * being numbered 10000 and higher, while the XML layer's codes are 9999 and
 * lower.
 *
 * The following is a table of the symbolic names of SBMLErrorCode_t values
 * and the meaning of each code.  In this table, the right-hand columns
 * titled "L1V1", "L1V2", etc. refer to Levels and Versions of the SBML
 * specifications, and the entries in each column refer to whether the
 * severity of the condition in that particular Level+Version of SBML.
 * The codes stand for the following:
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
 * getShortMessage() method on a given SBMLError object.  A longer and
 * (hopefully) clearer explanation of the issue is returned by
 * getMessage().
 * 
 * <center>
 * <table width="95%" cellspacing="1" cellpadding="2" border="0" class="small-font alt-row-colors">
 *  <tr style="background: lightgray" class="normal-font">
 *      <th valign="bottom"><strong>Enumerator</strong></th>
 *      <th valign="bottom"><strong>Meaning</strong></th>
 *      <th align="center" width="15">L 1 V 1</th>
 *      <th align="center" width="15">L 1 V 2</th>
 *      <th align="center" width="15">L 2 V 1</th>
 *      <th align="center" width="15">L 2 V 2</th>
 *      <th align="center" width="15">L 2 V 3</th>
 *      <th align="center" width="15">L 2 V 4</th>
 *  </tr>
 * <tr><td><em>UnknownError</em></td><td>Unknown internal libSBML error</td><td class="s-fatal">F</td><td class="s-fatal">F</td><td class="s-fatal">F</td><td class="s-fatal">F</td><td class="s-fatal">F</td><td class="s-fatal">F</td></tr>
 * <tr><td><em>NotUTF8</em></td><td>Not UTF8</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>UnrecognizedElement</em></td><td>Unrecognized element</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NotSchemaConformant</em></td><td>Not conformant to SBML XML schema</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidMathElement</em></td><td>Invalid MathML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>DisallowedMathMLSymbol</em></td><td>Disallowed MathML symbol</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>DisallowedMathMLEncodingUse</em></td><td>Disallowed use of MathML <code>encoding</code> attribute</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>DisallowedDefinitionURLUse</em></td><td>Disallowed use of MathML <code>definitionURL</code> attribute</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>BadCsymbolDefinitionURLValue</em></td><td>Invalid <code>&lt;csymbol&gt;</code> <code>definitionURL</code> attribute value</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>DisallowedMathTypeAttributeUse</em></td><td>Disallowed use of MathML <code>type</code> attribute</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>DisallowedMathTypeAttributeValue</em></td><td>Disallowed MathML <code>type</code> attribute value</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>LambdaOnlyAllowedInFunctionDef</em></td><td>Use of <code>&lt;lambda&gt;</code> not permitted outside of a <code>&lt;functionDefinition&gt;</code></td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>BooleanOpsNeedBooleanArgs</em></td><td>Non-boolean argument given to boolean operator</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NumericOpsNeedNumericArgs</em></td><td>Non-numerical argument given to numerical operator</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>ArgsToEqNeedSameType</em></td><td>Arguments to <code>&lt;eq&gt;</code> or <code>&lt;neq&gt;</code> have inconsistent data types</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>PiecewiseNeedsConsistentTypes</em></td><td><code>&lt;piecewise&gt;</code> terms have inconsistent data types</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>PieceNeedsBoolean</em></td><td>Second argument of <code>&lt;piece&gt;</code> must yield a boolean value</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>ApplyCiMustBeUserFunction</em></td><td><code>&lt;ci&gt;</code> does not refer to a function definition</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>ApplyCiMustBeModelComponent</em></td><td><code>&lt;ci&gt;</code>'s value is not a component in this model</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>KineticLawParametersAreLocalOnly</em></td><td>Cannot use <code>&lt;kineticLaw&gt;</code> parameter outside local scope</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>MathResultMustBeNumeric</em></td><td>Formula result is not a numerical value</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>OpsNeedCorrectNumberOfArgs</em></td><td>Incorrect number of arguments to operator</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidNoArgsPassedToFunctionDef</em></td><td>Incorrect number of arguments to function</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td></tr>
 * <tr><td><em>DuplicateComponentId</em></td><td>Duplicate component identifier</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>DuplicateUnitDefinitionId</em></td><td>Duplicate unit definition identifier</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>DuplicateLocalParameterId</em></td><td>Duplicate local parameter identifier</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>MultipleAssignmentOrRateRules</em></td><td>Multiple rules for the same variable</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>MultipleEventAssignmentsForId</em></td><td>Multiple event assignments for the same variable</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>EventAndAssignmentRuleForId</em></td><td><code>variable</code> value used in both event assignments and assignment rules</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>DuplicateMetaId</em></td><td>Duplicate <code>metaid</code> identifier</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidSBOTermSyntax</em></td><td>Invalid <code>sboTerm</code> value syntax</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidMetaidSyntax</em></td><td>Invalid <code>metaid</code> value syntax</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidIdSyntax</em></td><td>Invalid identifier syntax</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidUnitIdSyntax</em></td><td>Invalid unit identifier syntax</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>MissingAnnotationNamespace</em></td><td>Missing declaration of XML namespace for annotation</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>DuplicateAnnotationNamespaces</em></td><td>Multiple annotations using same XML namespace</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>SBMLNamespaceInAnnotation</em></td><td>Invalid use of SBML XML namespace in annotation</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InconsistentArgUnits</em></td><td>Units of arguments to function call do not match function's definition</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>AssignRuleCompartmentMismatch</em></td><td>Mismatched units in assignment rule for compartment</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>AssignRuleSpeciesMismatch</em></td><td>Mismatched units in assignment rule for species</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>AssignRuleParameterMismatch</em></td><td>Mismatched units in assignment rule for parameter</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InitAssignCompartmenMismatch</em></td><td>Mismatched units in initial assignment to compartment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InitAssignSpeciesMismatch</em></td><td>Mismatched units in initial assignment to species</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InitAssignParameterMismatch</em></td><td>Mismatched units in initial assignment to parameter</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>RateRuleCompartmentMismatch</em></td><td>Mismatched units in rate rule for compartment</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>RateRuleSpeciesMismatch</em></td><td>Mismatched units in rate rule for species</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>RateRuleParameterMismatch</em></td><td>Mismatched units in rate rule for parameter</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>KineticLawNotSubstancePerTime</em></td><td>Kinetic law units are not <code>substance</code>/<code>time</code></td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>DelayUnitsNotTime</em></td><td>Units of delay are not units of time</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>EventAssignCompartmentMismatch</em></td><td>Mismatched units in event assignment for compartment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>EventAssignSpeciesMismatch</em></td><td>Mismatched units in event assignment for species</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>EventAssignParameterMismatch</em></td><td>Mismatched units in event assignment for parameter</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>OverdeterminedSystem</em></td><td>Model is overdetermined</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidModelSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for model</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidFunctionDefSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for function definition</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidParameterSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for parameter</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidInitAssignSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for initial assignment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidRuleSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for rule</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidConstraintSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for constraint</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidReactionSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for reaction</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidSpeciesReferenceSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for species reference</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidKineticLawSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for kinetic law</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidEventSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for event</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidEventAssignmentSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for event assignment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidCompartmentSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for compartment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidSpeciesSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for species</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidCompartmentTypeSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for compartment type</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidSpeciesTypeSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for species type</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidTriggerSBOTerm</em></td><td>Invalid <code>sboTerm</code> value for event trigger</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>InvalidDelaySBOTerm</em></td><td>Invalid <code>sboTerm</code> value for event delay</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-warning">W</td></tr>
 * <tr><td><em>NotesNotInXHTMLNamespace</em></td><td>Notes not placed in XHTML namespace</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NotesContainsXMLDecl</em></td><td>XML declarations not permitted in notes</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NotesContainsDOCTYPE</em></td><td>XML <code>DOCTYPE</code> not permitted in notes</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidNotesContent</em></td><td>Invalid notes content</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidNamespaceOnSBML</em></td><td>Invalid XML namespace for SBML container</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>MissingOrInconsistentLevel</em></td><td>Missing or inconsistent value for <code>level</code> attribute</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>MissingOrInconsistentVersion</em></td><td>Missing or inconsistent value for <code>version</code> attribute</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>AnnotationNotesNotAllowedLevel1</em></td><td>Annotation on <code>&lt;sbml&gt;</code> not permitted in SBML Level 1</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>MissingModel</em></td><td>Missing model</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>IncorrectOrderInModel</em></td><td>Incorrect ordering of components in model definition</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>EmptyListElement</em></td><td>A given <code>listOf___</code>, if present, cannot be empty</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NeedCompartmentIfHaveSpecies</em></td><td>Missing compartment in species definition</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>FunctionDefMathNotLambda</em></td><td>Invalid expression in function definition</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidApplyCiInLambda</em></td><td>Invalid forward reference in <code>&lt;apply&gt;</code><code>&lt;ci&gt;</code>...<code>&lt;/ci&gt;</code><code>&lt;/apply&gt;</code> value</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td></tr>
 * <tr><td><em>RecursiveFunctionDefinition</em></td><td>Recursive function definition</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidCiInLambda</em></td><td>Unknown <code>&lt;ci&gt;</code> reference in <code>&lt;lambda&gt;</code></td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidFunctionDefReturnType</em></td><td>Function return type must be either numerical or boolean</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidUnitDefId</em></td><td>Invalid <code>id</code> value for unit definition</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidSubstanceRedefinition</em></td><td>Invalid redefinition of <code>substance</code></td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidLengthRedefinition</em></td><td>Invalid redefinition of <code>length</code></td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidAreaRedefinition</em></td><td>Invalid redefinition of <code>area</code></td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidTimeRedefinition</em></td><td>Invalid redefinition of <code>time</code></td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidVolumeRedefinition</em></td><td>Invalid redefinition of <code>volume</code></td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>VolumeLitreDefExponentNotOne</em></td><td>Must use <code>exponent</code>=<code>1</code> when defining <code>volume</code> in terms of litres</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td></tr>
 * <tr><td><em>VolumeMetreDefExponentNot3</em></td><td>Must use <code>exponent</code>=<code>3</code> when defining <code>volume</code> in terms of metres</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td></tr>
 * <tr><td><em>EmptyListOfUnits</em></td><td>Empty list of units not permitted</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidUnitKind</em></td><td>Invalid value of <code>kind</code> in unit definition</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>OffsetNoLongerValid</em></td><td><code>offset</code> not supported in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td></tr>
 * <tr><td><em>CelsiusNoLongerValid</em></td><td><code>Celsius</code> not defined in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td></tr>
 * <tr><td><em>ZeroDimensionalCompartmentSize</em></td><td>Use of <code>size</code> is invalid for a zero-dimensional compartment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>ZeroDimensionalCompartmentUnits</em></td><td>Use of <code>units</code> is invalid for a zero-dimensional compartment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>ZeroDimensionalCompartmentConst</em></td><td>Zero-dimensional compartments cannot be non-constant</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>UndefinedOutsideCompartment</em></td><td>Undefined compartment used as <code>outside</code> value</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>RecursiveCompartmentContainment</em></td><td>Recursive nesting of compartments via <code>outside</code></td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>ZeroDCompartmentContainment</em></td><td>Invalid nesting of zero-dimensional compartments</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>Invalid1DCompartmentUnits</em></td><td>Invalid value of <code>units</code> for a one-dimensional compartment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>Invalid2DCompartmentUnits</em></td><td>Invalid value of <code>units</code> for a two-dimensional compartment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>Invalid3DCompartmentUnits</em></td><td>Invalid value of <code>units</code> for a three-dimensional compartment</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidCompartmentTypeRef</em></td><td>Invalid <code>compartmentType</code> reference</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidSpeciesCompartmentRef</em></td><td>Invalid <code>compartment</code> reference</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>HasOnlySubsNoSpatialUnits</em></td><td>No <code>spatialSizeUnits</code> permitted if <code>hasOnlySubstanceUnits</code>=<code>true</code></td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NoSpatialUnitsInZeroD</em></td><td>No <code>spatialSizeUnits</code> permitted if compartment is zero-dimensional</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NoConcentrationInZeroD</em></td><td>No <code>initialConcentration</code> permitted if compartment is zero-dimensional</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>SpatialUnitsInOneD</em></td><td>Invalid value of <code>spatialSizeUnits</code> for a one-dimensional compartment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>SpatialUnitsInTwoD</em></td><td>Invalid value of <code>spatialSizeUnits</code> for a two-dimensional compartment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>SpatialUnitsInThreeD</em></td><td>Invalid value of <code>spatialSizeUnits</code> for a three-dimensional compartment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>InvalidSpeciesSusbstanceUnits</em></td><td>Invalid value of <code>units</code></td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>BothAmountAndConcentrationSet</em></td><td>Cannot set both <code>initialConcentration</code> and <code>initialAmount</code></td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NonBoundarySpeciesAssignedAndUsed</em></td><td>Cannot use non-boundary species in both reactions and rules simultaneously</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NonConstantSpeciesUsed</em></td><td>Cannot use non-boundary, constant species as reactant or product</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidSpeciesTypeRef</em></td><td>Invalid <code>speciesType</code> reference</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>MultSpeciesSameTypeInCompartment</em></td><td>Cannot have multiple species of the same type in the same compartment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>MissingSpeciesCompartment</em></td><td>Missing <code>compartment</code> value for species</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>SpatialSizeUnitsRemoved</em></td><td>Attribute <code>spatialSizeUnits</code> not supported in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidParameterUnits</em></td><td>Invalid value for <code>units</code> in parameter definition</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidInitAssignSymbol</em></td><td>Invalid <code>symbol</code> reference in initial assignment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>MultipleInitAssignments</em></td><td>Multiple initial assignments for the same <code>symbol</code> value</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InitAssignmentAndRuleForSameId</em></td><td>Cannot set a value with both initial assignments and assignment rules simultaneously</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidAssignRuleVariable</em></td><td>Invalid <code>variable</code> reference in assignment rule</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidRateRuleVariable</em></td><td>Invalid <code>variable</code> reference in rate rule</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>AssignmentToConstantEntity</em></td><td>Cannot reassign a constant in an assignment rule</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>RateRuleForConstantEntity</em></td><td>Cannot reassign a constant in a rate rule</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>CircularRuleDependency</em></td><td>Circular dependency involving rules and reactions</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>ConstraintMathNotBoolean</em></td><td>Non-boolean math expression in constraint definition</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>IncorrectOrderInConstraint</em></td><td>Incorrect order of elements in constraint definition</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>ConstraintNotInXHTMLNamespace</em></td><td>Constraint message is not in XHTML XML namespace</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>ConstraintContainsXMLDecl</em></td><td>XML declarations not permitted in constraint messages</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>ConstraintContainsDOCTYPE</em></td><td>XML <code>DOCTYPE</code> not permitted in constraint messages</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidConstraintContent</em></td><td>Invalid content for constraint message</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NoReactantsOrProducts</em></td><td>Cannot have a reaction with neither reactants nor products</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>IncorrectOrderInReaction</em></td><td>Incorrect ordering of components in reaction definition</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>EmptyListInReaction</em></td><td>Reaction components, if present, cannot be empty</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidReactantsProductsList</em></td><td>Invalid element in list of reactants or products</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidModifiersList</em></td><td>Invalid element in list of modifiers</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidSpeciesReference</em></td><td>Invalid <code>species</code> value in species reference</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>BothStoichiometryAndMath</em></td><td>Cannot use both <code>stoichiometry</code> and <code>&lt;stoichiometryMath&gt;</code> simultaneously</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>UndeclaredSpeciesRef</em></td><td>Undeclared species referenced in kinetic law formula</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>IncorrectOrderInKineticLaw</em></td><td>Incorrect ordering of components in kinetic law definition</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>EmptyListInKineticLaw</em></td><td>The list of parameters component, if present, cannot be empty</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NonConstantLocalParameter</em></td><td>Parameters local to a kinetic law must have <code>constant</code>=<code>true</code></td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>SubsUnitsNoLongerValid</em></td><td><code>substanceUnits</code> not supported in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td></tr>
 * <tr><td><em>TimeUnitsNoLongerValid</em></td><td><code>timeUnits</code> not supported in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td></tr>
 * <tr><td><em>UndeclaredSpeciesInStoichMath</em></td><td>Undeclared species referenced in <code>&lt;stoichiometryMath&gt;</code> formula</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>MissingTriggerInEvent</em></td><td>Missing trigger in event definition</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>TriggerMathNotBoolean</em></td><td>Non-boolean math expression in trigger definition</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>MissingEventAssignment</em></td><td>Missing event assignment in event definition</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>TimeUnitsEvent</em></td><td>Units of <code>timeUnits</code> are not time units</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>IncorrectOrderInEvent</em></td><td>Incorrect ordering of components in event definition</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>ValuesFromTriggerTimeNeedDelay</em></td><td><code>useValuesFromTriggerTime</code>=<code>false</code>, but no delay defined in event</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidEventAssignmentVariable</em></td><td>Invalid value for <code>variable</code> in event assignment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>EventAssignmentForConstantEntity</em></td><td>Cannot assign to a constant component in an event assignment</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>CompartmentShouldHaveSize</em></td><td>It's best to define a size for every compartment in a model</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>ParameterShouldHaveUnits</em></td><td>It's best to declare units for every parameter in a model</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>LocalParameterShadowsId</em></td><td>Local parameters defined in a kinetic law shadow global parameters</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>CannotConvertToL1V1</em></td><td>Cannot convert to SBML Level 1 Version&nbsp;1</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NoEventsInL1</em></td><td>SBML Level 1 does not support events</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NoFunctionDefinitionsInL1</em></td><td>SBML Level 1 does not support function definitions</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NoConstraintsInL1</em></td><td>SBML Level 1 does not support constraints</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>NoInitialAssignmentsInL1</em></td><td>SBML Level 1 does not support initial assignments</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NoSpeciesTypesInL1</em></td><td>SBML Level 1 does not support species types</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>NoCompartmentTypeInL1</em></td><td>SBML Level 1 does not support compartment types</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>NoNon3DComparmentsInL1</em></td><td>SBML Level 1 only supports three-dimensional compartments</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NoFancyStoichiometryMathInL1</em></td><td>SBML Level 1 does not support non-integer nor non-rational stoichiometry formulas</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NoNonIntegerStoichiometryInL1</em></td><td>SBML Level 1 does not support non-integer <code>stoichiometry</code> attribute values</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NoUnitMultipliersOrOffsetsInL1</em></td><td>SBML Level 1 does not support multipliers or offsets in unit definitions</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>SpeciesCompartmentRequiredInL1</em></td><td>In SBML Level 1, a value for <code>compartment</code> is mandatory in species definitions</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NoSpeciesSpatialSizeUnitsInL1</em></td><td>SBML Level 1 does not support species <code>spatialSizeUnits</code> settings</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NoSBOTermsInL1</em></td><td>SBML Level 1 does not support the <code>sboTerm</code> attribute</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>NoConstraintsInL2v1</em></td><td>SBML Level&nbsp;2 Version&nbsp;1 does not support constraints</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>NoInitialAssignmentsInL2v1</em></td><td>SBML Level&nbsp;2 Version&nbsp;1 does not support initial assignments</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>NoSpeciesTypeInL2v1</em></td><td>SBML Level&nbsp;2 Version&nbsp;1 does not support species types</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>NoCompartmentTypeInL2v1</em></td><td>SBML Level&nbsp;2 Version&nbsp;1 does not support compartment types</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>NoSBOTermsInL2v1</em></td><td>SBML Level&nbsp;2 Version&nbsp;1 does not support the <code>sboTerm</code> attribute</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>NoIdOnSpeciesReferenceInL2v1</em></td><td>SBML Level&nbsp;2 Version&nbsp;1 does not support the <code>id</code> attribute on species references</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>NoDelayedEventAssignmentInL2v1</em></td><td>Attribute <code>useValuesFromTriggerTime</code> not supported in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td></tr>
 * <tr><td><em>SBOTermNotUniversalInL2v2</em></td><td>The <code>sboTerm</code> attribute is invalid for this component in Level&nbsp;2 Version&nbsp;2</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>NoUnitOffsetInL2v2</em></td><td>The unit <code>offset</code> attribute is invalid in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NoKineticLawTimeUnitsInL2v2</em></td><td>The <code>timeUnits</code> attribute is invalid in this Level+Version of SBML</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NoKineticLawSubstanceUnitsInL2v2</em></td><td>The <code>substanceUnits</code> attribute is invalid in this Level+Version of SBML</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NoDelayedEventAssignmentInL2v2</em></td><td>Attribute <code>useValuesFromTriggerTime</code> not supported in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td></tr>
 * <tr><td><em>ModelSBOBranchChangedBeyondL2v2</em></td><td>The allowable <code>sboTerm</code> values for model differ for this SBML Level+Version</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td></tr>
 * <tr><td><em>NoUnitOffsetInL2v3</em></td><td>Attribute <code>offset</code> not supported in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NoKineticLawTimeUnitsInL2v3</em></td><td>Attribute <code>timeUnits</code> not supported in this Level+Version of SBML</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NoKineticLawSubstanceUnitsInL2v3</em></td><td>Attribute <code>substanceUnits</code> not supported in this Level+Version of SBML</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NoSpeciesSpatialSizeUnitsInL2v3</em></td><td>Attribute <code>spatialSizeUnits</code> not supported in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NoEventTimeUnitsInL2v3</em></td><td>Attribute <code>timeUnits</code> not supported in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NoDelayedEventAssignmentInL2v3</em></td><td>Attribute <code>useValuesFromTriggerTime</code> not supported in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td></tr>
 * <tr><td><em>ModelSBOBranchChangedBeyondL2v3</em></td><td>The allowable <code>sboTerm</code> values for model differ for this SBML Level+Version</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td></tr>
 * <tr><td><em>NoUnitOffsetInL2v4</em></td><td>The unit <code>offset</code> attribute is invalid in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NoKineticLawTimeUnitsInL2v4</em></td><td>The <code>timeUnits</code> attribute is invalid in this Level+Version of SBML</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NoKineticLawSubstanceUnitsInL2v4</em></td><td>The <code>substanceUnits</code> attribute is invalid in this Level+Version of SBML</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NoSpeciesSpatialSizeUnitsInL2v4</em></td><td>The <code>spatialSizeUnits</code> attribute is invalid in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NoEventTimeUnitsInL2v4</em></td><td>The <code>timeUnits</code> attribute is invalid in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>ModelSBOBranchChangedInL2v4</em></td><td>The allowable <code>sboTerm</code> values for model differ for this SBML Level+Version</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td></tr>
 * <tr><td><em>InvalidSBMLLevelVersion</em></td><td>Invalid SBML Level and Version</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>InvalidRuleOrdering</em></td><td>Invalid ordering of rules</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>SubsUnitsAllowedInKL</em></td><td>Disallowed value for attribute <code>substanceUnits</code></td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>TimeUnitsAllowedInKL</em></td><td>Disallowed value for attribute <code>timeUnits</code></td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>FormulaInLevel1KL</em></td><td>Only predefined functions are permitted in SBML Level 1 formulas</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>TimeUnitsRemoved</em></td><td>The <code>timeUnits</code> attribute is invalid in this Level+Version of SBML</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>BadMathML</em></td><td>Invalid MathML expression</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td>   </tr>
 * <tr><td><em>FailedMathMLReadOfDouble</em></td><td>Failed to read floating-point number</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td>   </tr>
 * <tr><td><em>FailedMathMLReadOfInteger</em></td><td>Failed to read an integer</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td>   </tr>
 * <tr><td><em>FailedMathMLReadOfExponential</em></td><td>Failed to read an exponential expression</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td>   </tr>
 * <tr><td><em>FailedMathMLReadOfRational</em></td><td>Failed to read a rational expression</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td>   </tr>
 * <tr><td><em>BadMathMLNodeType</em></td><td>Invalid MathML element</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td>   </tr>
 * <tr><td><em>NoTimeSymbolInFunctionDef</em></td><td><code>&lt;csymbol&gt;</code> for <code>time</code> used within the <code>&lt;math&gt;</code> of a function definition</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td>   </tr>
 * <tr><td><em>UndeclaredUnits</em></td><td>Undeclared units</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>UnrecognisedSBOTerm</em></td><td>Unrecognized <code>sboTerm</code> value</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>ObseleteSBOTerm</em></td><td>Obsolete <code>sboTerm</code> value</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-warning">W</td><td class="s-warning">W</td><td class="s-warning">W</td></tr>
 * <tr><td><em>IncorrectCompartmentSpatialDimensions</em></td><td>in SBML Level 1, only three-dimensional compartments are permitted</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>CompartmentTypeNotValidAttribute</em></td><td>Compartment types not supported in this Level+Version of SBML</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>ConstantNotValidAttribute</em></td><td>Attribute <code>constant</code> not supported on this component in SBML Level 1</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>MetaIdNotValidAttribute</em></td><td>Attribute <code>metaid</code> not supported in SBML Level 1</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>SBOTermNotValidAttributeBeforeL2V3</em></td><td><code>sboTerm</code> not available on this component before SBML Level&nbsp;2 Version&nbsp;3</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>InvalidL1CompartmentUnits</em></td><td>Invalid units for a compartment in SBML Level 1</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>L1V1CompartmentVolumeReqd</em></td><td>Compartment volume must be specified</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>CompartmentTypeNotValidComponent</em></td><td>Compartment types not supported in this Level+Version of SBML</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>ConstraintNotValidComponent</em></td><td>Constraints not supported in this Level+Version of SBML</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>EventNotValidComponent</em></td><td>Events not supported in this Level+Version of SBML</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>SBOTermNotValidAttributeBeforeL2V2</em></td><td>The <code>sboTerm</code> attribute is invalid for this component before Level&nbsp;2 Version&nbsp;2</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>FuncDefNotValidComponent</em></td><td>Function definitions are not supported in this Level+Version of SBML</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>InitialAssignNotValidComponent</em></td><td>Initial assignments are not supported in this Level+Version of SBML</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>VariableNotValidAttribute</em></td><td>Attribute <code>variable</code> not valid</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>UnitsNotValidAttribute</em></td><td>Attribute <code>units</code> not valid</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td></tr>
 * <tr><td><em>ConstantSpeciesNotValidAttribute</em></td><td>Attribute <code>constant</code> on species not supported in SBML Level 1</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>SpatialSizeUnitsNotValidAttribute</em></td><td>Attribute <code>spatialSizeUnits</code> on species not supported in SBML Level 1</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>SpeciesTypeNotValidAttribute</em></td><td>Attribute <code>speciesType</code> on species not supported in SBML Level 1</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>HasOnlySubsUnitsNotValidAttribute</em></td><td>Attribute <code>hasOnlySubstanceUnits</code> on species not supported in SBML Level 1</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>IdNotValidAttribute</em></td><td>Attribute <code>id</code> on species references not supported in SBML Level 1</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>NameNotValidAttribute</em></td><td>Attribute <code>name</code> on species references not supported in SBML Level 1</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>SpeciesTypeNotValidComponent</em></td><td>Species types not supported in SBML Level 1</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>StoichiometryMathNotValidComponent</em></td><td>lt;stoichiometryMath&gt; not supported in SBML Level 1</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * <tr><td><em>MultiplierNotValidAttribute</em></td><td>Attribute <code>multiplier</code> on units not supported in SBML Level 1</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr> 
 * <tr><td><em>OffsetNotValidAttribute</em></td><td>Attribute <code>offset</code> on units only available in SBML Level&nbsp;2 Version&nbsp;1</td><td class="s-error">E</td><td class="s-error">E</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td><td class="s-na">N</td></tr>
 * </table>
 * </center>
 *
 *
 * <h3><a class="anchor" name="SBMLErrorCategory_t">SBMLErrorCategory_t</a></h3>
 *
 * This is an enumeration of category codes for SBMLError diagnostics.
 * These enumeration values are distinct from (and in addition to) the
 * @link XMLError::XMLErrorCategory_t XMLErrorCategory_t @endlink codes
 * used by the parent XMLError object.  User programs receiving an
 * SBMLError object can use this distinction to check whether the error
 * represents a low-level XML problem or an SBML problem.
 * 
 * <center>
 * <table width="90%" cellspacing="1" cellpadding="4" border="0"  class="small-font alt-row-colors">
 *  <tr style="background: lightgray" class="normal-font">
 *      <th>Enumerator</td>
 *      <th>Meaning</td>
 *  </tr>
 * <tr><td><em>LIBSBML_CAT_SBML</em></td><td>General error not falling into
 * another category below.</td></tr> 
 * <tr><td><em>LIBSBML_CAT_SBML_L1_COMPAT</em></td><td>Category of errors
 * that can only occur during attempted translation from one Level/Version
 * of SBML to another.  This particular category applies to errors
 * encountered while trying to convert a model from SBML Level&nbsp;2 to SBML
 * Level&nbsp;1.</td></tr> 
 * <tr><td><em>LIBSBML_CAT_SBML_L2V1_COMPAT</em></td><td>Category of errors
 * that can only occur during attempted translation from one Level/Version
 * of SBML to another.  This particular category applies to errors
 * encountered while trying to convert a model to SBML Level&nbsp;2
 * Version&nbsp;1.</td></tr> 
 * <tr><td><em>LIBSBML_CAT_SBML_L2V2_COMPAT</em></td><td>Category of errors
 * that can only occur during attempted translation from one Level/Version
 * of SBML to another.  This particular category applies to errors
 * encountered while trying to convert a model to SBML Level&nbsp;2
 * Version&nbsp;2.</td></tr> 
 * <tr><td><em>LIBSBML_CAT_GENERAL_CONSISTENCY</em></td><td>Category of
 * errors that can occur while validating general SBML constructs.  With
 * respect to the SBML specification, these concern failures in applying
 * the validation rules numbered 2xxxx in the Level&nbsp;2 Versions&nbsp;2
 * and&nbsp;3 specifications.</td></tr>
 * <tr><td><em>LIBSBML_CAT_IDENTIFIER_CONSISTENCY</em></td><td>Category of
 * errors that can occur while validating symbol identifiers in a model.
 * With respect to the SBML specification, these concern failures in
 * applying the validation rules numbered 103xx in the Level&nbsp;2
 * Versions&nbsp;2 and&nbsp;3 specifications.</td></tr>  
 * <tr><td><em>LIBSBML_CAT_UNITS_CONSISTENCY</em></td><td>Category of
 * errors that can occur while validating the units of measurement on
 * quantities in a model.  With respect to the SBML specification, these
 * concern failures in applying the validation rules numbered 105xx in the
 * Level&nbsp;2 Versions&nbsp;2 and&nbsp;3 specifications.</td></tr> 
 * <tr><td><em>LIBSBML_CAT_MATHML_CONSISTENCY</em></td><td>Category of
 * errors that can occur while validating MathML formulas in a model.  With
 * respect to the SBML specification, these concern failures in applying
 * the validation rules numbered 102xx in the Level&nbsp;2 Versions&nbsp;2
 * and&nbsp;3 specifications.</td></tr> 
 * <tr><td><em>LIBSBML_CAT_SBO_CONSISTENCY</em></td><td>Category of errors
 * that can occur while validating SBO identifiers in a model.  With
 * respect to the SBML specification, these concern failures in applying
 * the validation rules numbered 107xx in the Level&nbsp;2 Versions&nbsp;2
 * and&nbsp;3 specifications.</td></tr> 
 * <tr><td><em>LIBSBML_CAT_OVERDETERMINED_MODEL</em></td><td>Error in the
 * system of equations in the model: the system is overdetermined,
 * therefore violating a tenet of proper SBML.  With respect to the SBML
 * specification, this is validation rule #10601 in the SBML Level&nbsp;2
 * Versions&nbsp;2 and&nbsp;3 specifications.</td></tr> 
 * <tr><td><em>LIBSBML_CAT_SBML_L2V3_COMPAT</em></td><td>Category of errors
 * that can only occur during attempted translation from one Level/Version
 * of SBML to another.  This particular category applies to errors
 * encountered while trying to convert a model to SBML Level&nbsp;2
 * Version&nbsp;3.</td></tr> 
 * <tr><td><em>LIBSBML_CAT_MODELING_PRACTICE</em></td><td>Category of
 * warnings about recommended good practices involving SBML and
 * computational modeling.  (These are tests performed by libSBML and do
 * not have equivalent SBML validation rules.)</td></tr> 
 * <tr><td><em>LIBSBML_CAT_INTERNAL_CONSISTENCY</em></td><td>Category of
 * errors that can occur while validating libSBML's internal representation
 * of SBML constructs. (These are tests performed by libSBML and do
 * not have equivalent SBML validation rules.)</td></tr> 
 * <tr><td><em>LIBSBML_CAT_SBML_L2V4_COMPAT</em></td><td>Category of errors
 * that can only occur during attempted translation from one Level/Version
 * of SBML to another.  This particular category applies to errors
 * encountered while trying to convert a model to SBML Level&nbsp;2
 * Version&nbsp;4.</td></tr> 
 *
 * </table>
 * </center>
 * 
 *
 * <h3><a class="anchor" name="SBMLErrorSeverity_t">SBMLErrorSeverity_t</a></h3>
 *
 * This is an enumeration of severity codes for SBMLError diagnostics.
 * These enumeration values are distinct from (and in addition to) the
 * @link XMLError::XMLErrorSeverity_t XMLErrorSeverity_t @endlink codes
 * used by the parent XMLError object.  User programs receiving an
 * SBMLError object can use this distinction to check whether the error
 * represents a low-level XML problem or an SBML problem.
 * 
 * In this verision of libSBML (3.3.x), there are no additional severity
 * codes in SBMLErrorSeverity_t beyond those defined in @link
 * XMLError::XMLErrorSeverity_t XMLErrorSeverity_t @endlink.
 */

#ifndef SBMLError_h
#define SBMLError_h

#include <sbml/common/extern.h>
#include <sbml/xml/XMLError.h>


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
 , MissingAnnotationNamespace            = 10401 /*!< Missing declaration of XML namespace for annotation */
 , DuplicateAnnotationNamespaces         = 10402 /*!< Multiple annotations using same XML namespace */
 , SBMLNamespaceInAnnotation             = 10403 /*!< Invalid use of SBML XML namespace in annotation */
 , InconsistentArgUnits                  = 10501 /*!< Units of arguments to function call do not match function's definition */
 , AssignRuleCompartmentMismatch         = 10511 /*!< Mismatched units in assignment rule for compartment */
 , AssignRuleSpeciesMismatch             = 10512 /*!< Mismatched units in assignment rule for species */
 , AssignRuleParameterMismatch           = 10513 /*!< Mismatched units in assignment rule for parameter */
 , InitAssignCompartmenMismatch          = 10521 /*!< Mismatched units in initial assignment to compartment */
 , InitAssignSpeciesMismatch             = 10522 /*!< Mismatched units in initial assignment to species */
 , InitAssignParameterMismatch           = 10523 /*!< Mismatched units in initial assignment to parameter */
 , RateRuleCompartmentMismatch           = 10531 /*!< Mismatched units in rate rule for compartment */
 , RateRuleSpeciesMismatch               = 10532 /*!< Mismatched units in rate rule for species */
 , RateRuleParameterMismatch             = 10533 /*!< Mismatched units in rate rule for parameter */
 , KineticLawNotSubstancePerTime         = 10541 /*!< Kinetic law units are not <code>substance</code>/<code>time</code> */
 , DelayUnitsNotTime                     = 10551 /*!< Units of delay are not units of time */
 , EventAssignCompartmentMismatch        = 10561 /*!< Mismatched units in event assignment for compartment */
 , EventAssignSpeciesMismatch            = 10562 /*!< Mismatched units in event assignment for species */
 , EventAssignParameterMismatch          = 10563 /*!< Mismatched units in event assignment for parameter */
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
 , InvalidNamespaceOnSBML                = 20101 /*!< Invalid XML namespace for SBML container */
 , MissingOrInconsistentLevel            = 20102 /*!< Missing or inconsistent value for <code>level</code> attribute */
 , MissingOrInconsistentVersion          = 20103 /*!< Missing or inconsistent value for <code>version</code> attribute */
 , AnnotationNotesNotAllowedLevel1       = 20104 /*!< Annotation on <code>&lt;sbml&gt;</code> not permitted in SBML Level&nbsp;1 */
 , MissingModel                          = 20201 /*!< Missing model */
 , IncorrectOrderInModel                 = 20202 /*!< Incorrect ordering of components in model definition */
 , EmptyListElement                      = 20203 /*!< A given <code>listOf___</code>, if present, cannot be empty */
 , NeedCompartmentIfHaveSpecies          = 20204 /*!< Missing compartment in species definition */
 , FunctionDefMathNotLambda              = 20301 /*!< Invalid expression in function definition */
 , InvalidApplyCiInLambda                = 20302 /*!< Invalid forward reference in <code>&lt;apply&gt;</code><code>&lt;ci&gt;</code>...<code>&lt;/ci&gt;</code><code>&lt;/apply&gt;</code> value */
 , RecursiveFunctionDefinition           = 20303 /*!< Recursive function definition */
 , InvalidCiInLambda                     = 20304 /*!< Unknown <code>&lt;ci&gt;</code> reference in <code>&lt;lambda&gt;</code> */
 , InvalidFunctionDefReturnType          = 20305 /*!< Function return type must be either numerical or boolean */
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
 , InvalidParameterUnits                 = 20701 /*!< Invalid value for <code>units</code> in parameter definition */
 , InvalidInitAssignSymbol               = 20801 /*!< Invalid <code>symbol</code> reference in initial assignment */
 , MultipleInitAssignments               = 20802 /*!< Multiple initial assignments for the same <code>symbol</code> value */
 , InitAssignmentAndRuleForSameId        = 20803 /*!< Cannot set a value with both initial assignments and assignment rules simultaneously */
 , InvalidAssignRuleVariable             = 20901 /*!< Invalid <code>variable</code> reference in assignment rule */
 , InvalidRateRuleVariable               = 20902 /*!< Invalid <code>variable</code> reference in rate rule */
 , AssignmentToConstantEntity            = 20903 /*!< Cannot reassign a constant in an assignment rule */
 , RateRuleForConstantEntity             = 20904 /*!< Cannot reassign a constant in a rate rule */
 , RepeatedRule10304                     = 20905 /*!<  */
 , CircularRuleDependency                = 20906 /*!< Circular dependency involving rules and reactions */
 , ConstraintMathNotBoolean              = 21001 /*!< Non-boolean math expression in constraint definition */
 , IncorrectOrderInConstraint            = 21002 /*!< Incorrect order of elements in constraint definition */
 , ConstraintNotInXHTMLNamespace         = 21003 /*!< Constraint message is not in XHTML XML namespace */
 , ConstraintContainsXMLDecl             = 21004 /*!< XML declarations not permitted in constraint messages */
 , ConstraintContainsDOCTYPE             = 21005 /*!< XML <code>DOCTYPE</code> not permitted in constraint messages */
 , InvalidConstraintContent              = 21006 /*!< Invalid content for constraint message */
 , NoReactantsOrProducts                 = 21101 /*!< Cannot have a reaction with neither reactants nor products */
 , IncorrectOrderInReaction              = 21102 /*!< Incorrect ordering of components in reaction definition */
 , EmptyListInReaction                   = 21103 /*!< Reaction components, if present, cannot be empty */
 , InvalidReactantsProductsList          = 21104 /*!< Invalid element in list of reactants or products */
 , InvalidModifiersList                  = 21105 /*!< Invalid element in list of modifiers */
 , InvalidSpeciesReference               = 21111 /*!< Invalid <code>species</code> value in species reference */
 , RepeatedRule20611                     = 21112 /*!<  */
 , BothStoichiometryAndMath              = 21113 /*!< Cannot use both <code>stoichiometry</code> and <code>&lt;stoichiometryMath&gt;</code> simultaneously */
 , UndeclaredSpeciesRef                  = 21121 /*!< Undeclared species referenced in kinetic law formula */
 , IncorrectOrderInKineticLaw            = 21122 /*!< Incorrect ordering of components in kinetic law definition */
 , EmptyListInKineticLaw                 = 21123 /*!< The list of parameters component, if present, cannot be empty */
 , NonConstantLocalParameter             = 21124 /*!< Parameters local to a kinetic law must have <code>constant</code>=<code>true</code> */
 , SubsUnitsNoLongerValid                = 21125 /*!< <code>substanceUnits</code> not supported in this Level+Version of SBML */
 , TimeUnitsNoLongerValid                = 21126 /*!< <code>timeUnits</code> not supported in this Level+Version of SBML */
 , UndeclaredSpeciesInStoichMath         = 21131 /*!< Undeclared species referenced in <code>&lt;stoichiometryMath&gt;</code> formula */
 , MissingTriggerInEvent                 = 21201 /*!< Missing trigger in event definition */
 , TriggerMathNotBoolean                 = 21202 /*!< Non-boolean math expression in trigger definition */
 , MissingEventAssignment                = 21203 /*!< Missing event assignment in event definition */
 , TimeUnitsEvent                        = 21204 /*!< Units of <code>timeUnits</code> are not time units */
 , IncorrectOrderInEvent                 = 21205 /*!< Incorrect ordering of components in event definition */
 , ValuesFromTriggerTimeNeedDelay        = 21206 /*!< <code>useValuesFromTriggerTime</code>=<code>false</code>, but no delay defined in event */
 , InvalidEventAssignmentVariable        = 21211 /*!< Invalid value for <code>variable</code> in event assignment */
 , EventAssignmentForConstantEntity      = 21212 /*!< Cannot assign to a constant component in an event assignment */

 , GeneralWarningNotSpecified            = 29999 /*!< Unknown error */

  /* ModelingPractice contraints */

 , CompartmentShouldHaveSize             = 80501 /*!< Compartment is missing size */
 , ParameterShouldHaveUnits              = 80701 /*!< Parameter definition should specify units */
 , LocalParameterShadowsId               = 81121 /*!< Parameter inside KineticLaw has same <code>id</code> as a global parameter */
    
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

  /* L2v1 compatability */

 , NoConstraintsInL2v1                   = 92001 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support constraints */
 , NoInitialAssignmentsInL2v1            = 92002 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support initial assignments */
 , NoSpeciesTypeInL2v1                   = 92003 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support species types */
 , NoCompartmentTypeInL2v1               = 92004 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support compartment types */
 , NoSBOTermsInL2v1                      = 92005 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support the <code>sboTerm</code> attribute */
 , NoIdOnSpeciesReferenceInL2v1          = 92006 /*!< SBML Level&nbsp;2 Version&nbsp;1 does not support the <code>id</code> attribute on species references */
 , NoDelayedEventAssignmentInL2v1        = 92007 /*!< Attribute <code>useValuesFromTriggerTime</code> not supported in this Level+Version of SBML */

  /* L2v2 compatability */

 , SBOTermNotUniversalInL2v2             = 93001 /*!< The <code>sboTerm</code> attribute is invalid for this component in Level&nbsp;2 Version&nbsp;2 */
 , NoUnitOffsetInL2v2                    = 93002 /*!< The unit <code>offset</code> attribute is invalid in this Level+Version of SBML */
 , NoKineticLawTimeUnitsInL2v2           = 93003 /*!< The <code>timeUnits</code> attribute is invalid in this Level+Version of SBML */
 , NoKineticLawSubstanceUnitsInL2v2      = 93004 /*!< The <code>substanceUnits</code> attribute is invalid in this Level+Version of SBML */
 , NoDelayedEventAssignmentInL2v2        = 93005 /*!< Attribute <code>useValuesFromTriggerTime</code> not supported in this Level+Version of SBML */
 , ModelSBOBranchChangedBeyondL2v2       = 93006 /*!< The allowable <code>sboTerm</code> values for model differ for this SBML Level+Version */

  /* L2v3 compatability  */

 , NoUnitOffsetInL2v3                    = 94001 /*!< Attribute <code>offset</code> not supported in this Level+Version of SBML */
 , NoKineticLawTimeUnitsInL2v3           = 94002 /*!< Attribute <code>timeUnits</code> not supported in this Level+Version of SBML */
 , NoKineticLawSubstanceUnitsInL2v3      = 94003 /*!< Attribute <code>substanceUnits</code> not supported in this Level+Version of SBML */
 , NoSpeciesSpatialSizeUnitsInL2v3       = 94004 /*!< Attribute <code>spatialSizeUnits</code> not supported in this Level+Version of SBML */
 , NoEventTimeUnitsInL2v3                = 94005 /*!< Attribute <code>timeUnits</code> not supported in this Level+Version of SBML */
 , NoDelayedEventAssignmentInL2v3        = 94006 /*!< Attribute <code>useValuesFromTriggerTime</code> not supported in this Level+Version of SBML */
 , ModelSBOBranchChangedBeyondL2v3       = 94007 /*!< The allowable <code>sboTerm</code> values for model differ for this SBML Level+Version */

  /* L2v4 compatability  */

 , NoUnitOffsetInL2v4                    = 95001 /*!< The unit <code>offset</code> attribute is invalid in this Level+Version of SBML */
 , NoKineticLawTimeUnitsInL2v4           = 95002 /*!< The <code>timeUnits</code> attribute is invalid in this Level+Version of SBML */
 , NoKineticLawSubstanceUnitsInL2v4      = 95003 /*!< The <code>substanceUnits</code> attribute is invalid in this Level+Version of SBML */
 , NoSpeciesSpatialSizeUnitsInL2v4       = 95004 /*!< The <code>spatialSizeUnits</code> attribute is invalid in this Level+Version of SBML */
 , NoEventTimeUnitsInL2v4                = 95005 /*!< The <code>timeUnits</code> attribute is invalid in this Level+Version of SBML */
 , ModelSBOBranchChangedInL2v4           = 95006 /*!< The allowable <code>sboTerm</code> values for model differ for this SBML Level+Version */

  /* These are errors checked by libSBML that were never
   * published in a spec. */

 , InvalidSBMLLevelVersion               = 99101 /*!< Invalid SBML Level and Version */
 , InvalidRuleOrdering                   = 99106 /*!< Invalid ordering of rules */

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


  /* These are internal errors that reverts to 10501. */

  /** @cond doxygen-libsbml-internal */
 , InconsistentArgUnitsWarnings          = 99502 /*!< SBML L2v3 validation rule #10501 */
 , InconsistentPowerUnitsWarnings        = 99503 /*!< SBML L2v3 validation rule #10501 */
 , InconsistentExponUnitsWarnings        = 99504 /*!< SBML L2v3 validation rule #10501 */
  /** @endcond doxygen-libsbml-internal */

 , UndeclaredUnits                       = 99505 /*!< Undeclared units */
 , UnrecognisedSBOTerm                   = 99701 /*!< Unrecognized <code>sboTerm</code> value */
 , ObseleteSBOTerm                       = 99702 /*!< Obsolete <code>sboTerm</code> value */

  /* Internal consistency checks */

 , IncorrectCompartmentSpatialDimensions = 99901 /*!< in SBML Level&nbsp;1, only three-dimensional compartments are permitted */
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

  /* Bounds */

  , SBMLCodesUpperBound                  = 99999 /*!< 99999, the upper bound of
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

  /** @endcond doxygen-libsbml-internal **/
} SBMLErrorSeverity_t;

END_C_DECLS


#ifdef __cplusplus

class LIBSBML_EXTERN SBMLError : public XMLError
{
public:

  /**
   * Creates a new SBMLError to report that something occurred during SBML
   * processing.
   *
   * SBMLError objects have identification numbers to indicate the nature
   * of the exception.  These numbers are drawn from the enumeration <a
   * class="el" href="#SBMLErrorCode_t">SBMLErrorCode_t</a>.  The argument
   * @p errorId to this constructor @em can be (but does not have to be) a
   * value from this enumeration.  If it is a value from <a class="el"
   * href="#SBMLErrorCode_t">SBMLErrorCode_t</a>, the SBMLError class
   * assumes it the error is an SBML error and prepends a predefined error
   * message to any string passed in @p details.  In addition, all <a
   * class="el" href="#SBMLErrorCode_t">SBMLErrorCode_t</a> errors have
   * associated severity and category codes, and these fields are filled-in
   * as well from the enumerations <a class="el"
   * href="#SBMLErrorSeverity_t">SBMLErrorSeverity_t</a> and <a class="el"
   * href="#SBMLErrorCategory_t">SBMLErrorCategory_t</a> respectively.
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
   * As mentioned above, there are two other enumerations, <a class="el"
   * href="#SBMLErrorSeverity_t">SBMLErrorSeverity_t</a> and <a class="el"
   * href="#SBMLErrorCategory_t">SBMLErrorCategory_t</a>, used for
   * indicating the severity and category of error for the predefined <a
   * class="el" href="#SBMLErrorCode_t">SBMLErrorCode_t</a> codes.  The
   * values passed in @p severity and @p category override the defaults
   * assigned based on the error code.  If the error identifier is a code
   * number from <a class="el" href="#SBMLErrorCode_t">SBMLErrorCode_t</a>,
   * callers do not need to fill in @p severity and @p category.
   * Conversely, if @p errorId is not a value from <a class="el"
   * href="#SBMLErrorCode_t">SBMLErrorCode_t</a>, callers can use other
   * values (not just those from <a class="el"
   * href="#SBMLErrorSeverity_t">SBMLErrorSeverity_t</a> and <a class="el"
   * href="#SBMLErrorCategory_t">SBMLErrorCategory_t</a>, but their own
   * special values) for @p severity and @p category.
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
   * @docnote The native C++ implementation of this method defines a
   * default argument value.  In the documentation generated for different
   * libSBML language bindings, you may or may not see corresponding
   * arguments in the method declarations.  For example, in Java, a default
   * argument is handled by declaring two separate methods, with one of
   * them having the argument and the other one lacking the argument.
   * However, the libSBML documentation will be @em identical for both
   * methods.  Consequently, if you are reading this and do not see an
   * argument even though one is described, please look for descriptions of
   * other variants of this method near where this one appears in the
   * documentation.
   */
  SBMLError
  (
     const unsigned int errorId  = 0
   , const unsigned int level    = 2
   , const unsigned int version  = 3
   , const std::string& details  = ""
   , const unsigned int line     = 0
   , const unsigned int column   = 0
   , const unsigned int severity = LIBSBML_SEV_ERROR
   , const unsigned int category = LIBSBML_CAT_SBML
  );

  /**
   * Copy Constructor
   */
  SBMLError(const SBMLError&);

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

  /** @endcond doxygen-libsbml-internal **/

#endif  /* !SWIG */

protected:
  /** @cond doxygen-libsbml-internal **/

  virtual std::string stringForSeverity(unsigned int code) const;
  virtual std::string stringForCategory(unsigned int code) const;

  /** @endcond doxygen-libsbml-internal **/
};

#endif  /* __cplusplus */


#endif /* SBMLError_h */
