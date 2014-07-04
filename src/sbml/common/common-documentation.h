/**
 * @file    common-documentation.h
 * @brief   Common text fragments used throughout libSBML's code documentation.
 * @author  Mike Hucka
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
 *
 * This file contains no code; it simply defines text fragments used as
 * common documentation blocks in other libSBML files via the @copydetails
 * operator from Doxygen.  The use of @@class is a hack needed because
 * Doxygen's @copydetails command has limited functionality.  No classes are
 * actually defined, and symbols beginning with "doc_" are marked as ignored
 * in our Doxygen configuration.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_is_listof
 *
 * @par
 * The various ListOf___ @if conly structures @else classes@endif@~ in SBML
 * are merely containers used for organizing the main components of an SBML
 * model.  In libSBML's implementation, ListOf___
 * @if conly data structures @else classes@endif@~ are derived from the
 * intermediate utility @if conly structure @else class@endif@~ ListOf, which
 * is not defined by the SBML specifications but serves as a useful
 * programmatic construct.  ListOf is itself is in turn derived from SBase,
 * which provides all of the various ListOf___
 * @if conly data structures @else classes@endif@~ with common features
 * defined by the SBML specification, such as "metaid" attributes and
 * annotations.
 *
 * The relationship between the lists and the rest of an SBML model is
 * illustrated by the following (for SBML Level&nbsp;2 Version&nbsp;4):
 *
 * @htmlinclude listof-illustration.html
 *
 * Readers may wonder about the motivations for using the ListOf___
 * containers in SBML.  A simpler approach in XML might be to place the
 * components all directly at the top level of the model definition.  The
 * choice made in SBML is to group them within XML elements named after
 * %ListOf<em>Classname</em>, in part because it helps organize the
 * components.  More importantly, the fact that the container classes are
 * derived from SBase means that software tools can add information @em about
 * the lists themselves into each list container's "annotation".
 *
 * @see ListOfFunctionDefinitions
 * @see ListOfUnitDefinitions
 * @see ListOfCompartmentTypes
 * @see ListOfSpeciesTypes
 * @see ListOfCompartments
 * @see ListOfSpecies
 * @see ListOfParameters
 * @see ListOfInitialAssignments
 * @see ListOfRules
 * @see ListOfConstraints
 * @see ListOfReactions
 * @see ListOfEvents
 *
 * @if conly
 * @note In the C API for libSBML, functions that in other language APIs
 * would be inherited by the various ListOf___ structures not shown in the
 * pages for the individual ListOf___'s.  Instead, the functions are defined
 * on ListOf_t.  <strong>Please consult the documentation for ListOf_t for
 * the many common functions available for manipulating ListOf___
 * structures</strong>.  The documentation for the individual ListOf___
 * structures (ListOfCompartments_t, ListOfReactions_t, etc.) does not reveal
 * all of the functionality available. @endif@~
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_are_sbmlnamespaces
 *
 * @par 
 * The SBMLNamespaces object encapsulates SBML Level/Version/namespaces
 * information.  It is used to communicate the SBML Level, Version, and (in
 * Level&nbsp;3) packages used in addition to SBML Level&nbsp;3 Core.  A
 * common approach to using libSBML's SBMLNamespaces facilities is to create an
 * SBMLNamespaces object somewhere in a program once, then hand that object
 * as needed to object constructors that accept SBMLNamespaces as arguments.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_is_SBMLDocument
 *
 * @par
 * LibSBML uses the class SBMLDocument as a top-level container for
 * storing SBML content and data associated with it (such as warnings and
 * error messages).  An SBML model in libSBML is contained inside an
 * SBMLDocument object.  SBMLDocument corresponds roughly to the class
 * <i>SBML</i> defined in the SBML Level&nbsp;3 and Level&nbsp;2
 * specifications, but it does not have a direct correspondence in SBML
 * Level&nbsp;1.  (But, it is created by libSBML no matter whether the
 * model is Level&nbsp;1, Level&nbsp;2 or Level&nbsp;3.)
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_is_metaid
 *
 * @par
 * The optional attribute named "metaid", present on every major SBML
 * component type, is for supporting metadata annotations using RDF (<a
 * href="http://www.w3.org/RDF/">Resource Description Format</a>).  The
 * attribute value has the data type <a
 * href="http://www.w3.org/TR/REC-xml/#id">XML <code>ID</code></a>, the XML
 * identifier type, which means each "metaid" value must be globally unique
 * within an SBML file.  The latter point is important, because the
 * uniqueness criterion applies across <em>any</em> attribute with type
 * <code>ID</code> anywhere in the file, not just the "metaid" attribute used
 * by SBML---something to be aware of if your application-specific XML
 * content inside the "annotation" subelement happens to use the XML
 * <code>ID</code> type.  Although SBML itself specifies the use of <a
 * href="http://www.w3.org/TR/REC-xml/#id">XML <code>ID</code></a> only for
 * the "metaid" attribute, SBML-compatible applications should be careful if
 * they use XML <code>ID</code>'s in XML portions of a model that are not
 * defined by SBML, such as in the application-specific content of the
 * "annotation" subelement.  Finally, note that LibSBML does not provide an
 * explicit XML <code>ID</code> data type; it uses ordinary character
 * strings, which is easier for applications to support.
 * 
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_is_sid
 *
 * @par
 * In SBML, identifiers that are the values of "id" attributes on objects
 * must conform to a data type called <code>SId</code> in the SBML
 * specifications.  LibSBML does not provide an explicit <code>SId</code>
 * data type; it uses ordinary character strings, which is easier for
 * applications to support.  (LibSBML does, however, test for identifier
 * validity at various times, such as when reading in models from files
 * and data streams.)
 *
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_is_sidref
 *
 * @par

 * In SBML, object identifiers are of a data type called <code>SId</code>.
 * In SBML Level&nbsp;3, an explicit data type called <code>SIdRef</code> was
 * introduced for attribute values that refer to <code>SId</code> values; in
 * previous Levels of SBML, this data type did not exist and attributes were
 * simply described to as "referring to an identifier", but the effective
 * data type was the same as <code>SIdRef</code>in Level&nbsp;3.  These and
 * other methods of libSBML refer to the type <code>SIdRef</code> for all
 * Levels of SBML, even if the corresponding SBML specification did not
 * explicitly name the data type.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_is_unitsidref
 *
 * @par
 * In SBML, unit definitions have identifiers of type <code>UnitSId</code>.  In
 * SBML Level&nbsp;3, an explicit data type called <code>UnitSIdRef</code> was
 * introduced for attribute values that refer to <code>UnitSId</code> values; in
 * previous Levels of SBML, this data type did not exist and attributes were
 * simply described to as "referring to a unit identifier", but the effective
 * data type was the same as <code>UnitSIdRef</code> in Level&nbsp;3.  These and
 * other methods of libSBML refer to the type <code>UnitSIdRef</code> for all
 * Levels of SBML, even if the corresponding SBML specification did not
 * explicitly name the data type.
 * 
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_is_metaidref
 *
 * @par
 * In SBML, object "meta" identifiers are of the XML data type <code>ID</code>;
 * the SBML object attribute itself is typically named <code>metaid</code>.  All
 * attributes that hold values <em>referring</em> to values of type
 * <code>ID</code> are of the XML data type <code>IDREF</code>.  They are also
 * sometimes informally referred to as "metaid refs", in analogy to the
 * SBML-defined type <code>SIdRef</code>.
 * 
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_id_syntax
 *
 * @par
 * SBML has strict requirements for the syntax of identifiers, that is, the
 * values of the "id" attribute present on most types of SBML objects.
 * The following is a summary of the definition of the SBML identifier type 
 * <code>SId</code>, which defines the permitted syntax of identifiers.  We
 * express the syntax using an extended form of BNF notation: 
 * <pre style="margin-left: 2em; border: none; font-weight: bold; font-size: 13px; color: black">
 * letter ::= 'a'..'z','A'..'Z'
 * digit  ::= '0'..'9'
 * idChar ::= letter | digit | '_'
 * SId    ::= ( letter | '_' ) idChar*</pre>
 * The characters <code>(</code> and <code>)</code> are used for grouping, the
 * character <code>*</code> "zero or more times", and the character
 * <code>|</code> indicates logical "or".  The equality of SBML identifiers is
 * determined by an exact character sequence match; i.e., comparisons must be
 * performed in a case-sensitive manner.  In addition, there are a few
 * conditions for the uniqueness of identifiers in an SBML model.  Please
 * consult the SBML specifications for the exact details of the uniqueness
 * requirements.
 * 
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_base_units
 *
 * @par
<table border="0" class="centered text-table width80 normal-font code"
       style="border: none !important">
<tr>
<td>ampere</td><td>farad</td><td>joule</td><td>lux</td><td>radian</td><td>volt</td>
</tr>
<tr>
<td>avogadro</td><td>gram</td><td>katal</td><td>metre</td><td>second</td><td>watt</td>
</tr>
<tr>
<td>becquerel</td><td>gray</td><td>kelvin</td><td>mole</td><td>siemens</td><td>weber</td>
</tr>
<tr>
<td>candela</td><td>henry</td><td>kilogram</td><td>newton</td><td>sievert</td>
</tr>
<tr>
<td>coulomb</td><td>hertz</td><td>litre</td><td>ohm</td><td>steradian</td>
</tr>
<tr>
<td>dimensionless</td><td>item</td><td>lumen</td><td>pascal</td><td>tesla</td>
</tr>
</table>
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_are_typecodes
 *
 * @par 
 * LibSBML attaches an identifying code to every kind of SBML object.  These
 * are integer constants known as <em>SBML type codes</em>.  The names of all
 * the codes begin with the characters &ldquo;<code>SBML_</code>&rdquo;. 
 * @if clike The set of possible type codes for core elements is defined in
 * the enumeration #SBMLTypeCode_t, and in addition, libSBML plug-ins for
 * SBML Level&nbsp;3 packages define their own extra enumerations of type
 * codes (e.g., #SBMLLayoutTypeCode_t for the Level&nbsp;3 Layout
 * package).@endif@if java In the Java language interface for libSBML, the
 * type codes are defined as static integer constants in the interface class
 * {@link libsbmlConstants}.  @endif@if python In the Python language
 * interface for libSBML, the type codes are defined as static integer
 * constants in the interface class @link libsbml@endlink.@endif@if csharp In
 * the C# language interface for libSBML, the type codes are defined as
 * static integer constants in the interface class
 * @link libsbmlcs.libsbml@endlink.@endif@~  Note that different Level&nbsp;3 
 * package plug-ins may use overlapping type codes; to identify the package
 * to which a given object belongs, call the <code>getPackageName()</code>
 * method on the object.
 * 
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_warning_typecodes_not_unique
 *
 * @warning <span class="warning">The specific integer values of the possible
 * type codes may be reused by different Level&nbsp;3 package plug-ins.
 * Thus, to identifiy the correct code, <strong>it is necessary to invoke
 * both getTypeCode() and getPackageName()</strong>.</span>
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_are_plugins
 *
 * @par
 * SBML Level&nbsp;3 consists of a <em>Core</em> definition that can be extended
 * via optional SBML Level&nbsp;3 <em>packages</em>.  A given model may indicate
 * that it uses one or more SBML packages, and likewise, a software tool may be
 * able to support one or more packages.  LibSBML does not come preconfigured
 * with all possible packages included and enabled, in part because not all
 * package specifications have been finalized.  To support the ability for
 * software systems to enable support for the Level&nbsp;3 packages they choose,
 * libSBML features a <em>plug-in</em> mechanism.  Each SBML Level&nbsp;3
 * package is implemented in a separate code plug-in that can be enabled by the
 * application to support working with that SBML package.  A given SBML model
 * may thus contain not only objects defined by SBML Level&nbsp;3 Core, but also
 * objects created by libSBML plug-ins supporting additional Level&nbsp;3
 * packages.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_are_notes
 *
 * @par
 * The optional SBML element named "notes", present on every major SBML
 * component type (and in SBML Level&nbsp;3, the "message" subelement of
 * Constraint), is intended as a place for storing optional information
 * intended to be seen by humans.  An example use of the "notes" element
 * would be to contain formatted user comments about the model element in
 * which the "notes" element is enclosed.  Every object derived directly or
 * indirectly from type SBase can have a separate value for "notes", allowing
 * users considerable freedom when adding comments to their models.
 *
 * The format of "notes" elements conform to the definition of <a
 * target="_blank" href="http://www.w3.org/TR/xhtml1/">XHTML&nbsp;1.0</a>.
 * However, the content cannot be @em entirely free-form; it must satisfy
 * certain requirements defined in the <a target="_blank"
 * href="http://sbml.org/Documents/Specifications">SBML specifications</a>
 * for specific SBML Levels.  To help verify the formatting of "notes"
 * content, libSBML provides the static utility method
 * SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif); this
 * method implements a verification process that lets callers check whether
 * the content of a given XMLNode object conforms to the SBML requirements
 * for "notes" and "message" structure.  Developers are urged to consult the
 * appropriate <a target="_blank"
 * href="http://sbml.org/Documents/Specifications">SBML specification
 * document</a> for the Level and Version of their model for more in-depth
 * explanations of using "notes" in SBML.  The SBML Level&nbsp;2 and &nbsp;3
 * specifications have considerable detail about how "notes" element content
 * must be structured.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_are_annotations
 *
 * @par
 * Whereas the SBML "notes" subelement is a container for content to be
 * shown directly to humans, the "annotation" element is a container for
 * optional software-generated content @em not meant to be shown to
 * humans.  Every object derived from SBase can have its own value for
 * "annotation".  The element's content type is <a target="_blank"
 * href="http://www.w3.org/TR/2004/REC-xml-20040204/#elemdecls">XML type
 * "any"</a>, allowing essentially arbitrary well-formed XML data
 * content.
 *
 * SBML places a few restrictions on the organization of the content of
 * annotations; these are intended to help software tools read and write
 * the data as well as help reduce conflicts between annotations added by
 * different tools.  Please see the SBML specifications for more details.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_are_cvterms
 *
 * @par
 * The SBML Level&nbsp;2 and Level&nbsp;3 specifications define a simple
 * format for annotating models when (a) referring to controlled
 * vocabulary terms and database identifiers that define and describe
 * biological and other entities, and (b) describing the creator of a
 * model and the model's modification history.  The annotation content is
 * stored in <code>&lt;annotation&gt;</code> elements attached to
 * individual SBML elements.  The format for storing the content inside
 * SBML <code>&lt;annotation&gt;</code> elements is a subset of W3C RDF
 * (<a target="_blank" href="http://www.w3.org/RDF/">Resource Description
 * Format</a>) expressed in XML.  The CVTerm class provides a programming
 * interface for working directly with controlled vocabulary term ("CV
 * term") objects without having to deal directly with the XML form.
 * When libSBML reads in an SBML model containing RDF annotations, it
 * parses those annotations into a list of CVTerm objects, and when
 * writing a model, it parses the CVTerm objects back into the
 * appropriate SBML <code>&lt;annotation&gt;</code> structure.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_is_astnode
 *
 * @par
 * An AST @em node in libSBML is a recursive structure containing a pointer
 * to the node's value (which might be, for example, a number or a symbol)
 * and a list of children nodes.  Each ASTNode node may have none, one,
 * two, or more children depending on its type.  The following diagram
 * illustrates an example of how the mathematical expression <code>"1 +
 * 2"</code> is represented as an AST with one @em plus node having two @em
 * integer children nodes for the numbers <code>1</code> and
 * <code>2</code>.  The figure also shows the corresponding MathML
 * representation:
 *
 * @htmlinclude astnode-illustration.html
 *
 * The following are other noteworthy points about the AST representation
 * in libSBML:

 * @li A numerical value represented in MathML as a real number with an
 * exponent is preserved as such in the AST node representation, even if
 * the number could be stored in a
 * @if python @c float@endif@if clike @c double @endif data type.  This is
 * done so that when an SBML model is read in and then written out again, the
 * amount of change introduced by libSBML to the SBML during the round-trip
 * activity is minimized.
 *
 * @li Rational numbers are represented in an AST node using separate
 * numerator and denominator values.  These can be retrieved using the
 * methods ASTNode::getNumerator() and ASTNode::getDenominator().
 *
 * @li The children of an ASTNode are other ASTNode objects.  The list of
 * children is empty for nodes that are leaf elements, such as numbers.
 * For nodes that are actually roots of expression subtrees, the list of
 * children points to the parsed objects that make up the rest of the
 * expression.
 *
 * For many applications, the details of ASTs are irrelevant because libSBML
 * provides text-string based translation functions such as
 * @sbmlfunction{formulaToL3String, ASTNode tree} and
 * @sbmlfunction{parseL3Formula, String formula}.  If you find the complexity
 * of using the AST representation of expressions too high for your purposes,
 * perhaps the string-based functions will be more suitable.
 *
 * Finally, it is worth noting that the AST and MathML handling code in
 * libSBML remains written in C, not C++.  (All of libSBML was originally
 * written in C.)  Readers may occasionally wonder why some aspects are more
 * C-like and less object oriented, and that's one of the reasons.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_astnode_types
 *
 * @par
 * Every ASTNode has an associated type code to indicate whether, for
 * example, it holds a number or stands for an arithmetic operator.
 * @if clike The type is recorded as a value drawn from the enumeration
 * @link ASTNode.h::ASTNodeType_t <code>ASTNodeType_t</code>@endlink.@endif
 * @if java The type is recorded as a value drawn from a
 * set of static integer constants defined in the class @link
 * libsbmlConstants@endlink. Their names begin with the characters @c AST_.@endif
 * @if python The type is recorded as a value drawn from a
 * set of static integer constants defined in the class @link
 * libsbml@endlink. Their names begin with the characters @c AST_.@endif
 * @if csharp The type is recorded as a value drawn from a
 * set of static integer constants defined in the class @link
 * libsbml@endlink. Their names begin with the characters @c AST_.@endif
 * The list of possible types is quite long, because it covers all the
 * mathematical functions that are permitted in SBML. The values are shown
 * in the following table:
 *
 * @htmlinclude astnode-types.html
 *
 * The types have the following meanings:
 *
 * @li If the node is basic mathematical operator (e.g., @c "+"), then the
 * node's type will be @sbmlconstant{AST_PLUS,ASTNodeType_t},
 * @sbmlconstant{AST_MINUS, ASTNodeType_t},
 * @sbmlconstant{AST_TIMES, ASTNodeType_t},
 * @sbmlconstant{AST_DIVIDE, ASTNodeType_t}, or
 * @sbmlconstant{AST_POWER, ASTNodeType_t}, as appropriate.
 *
 * @li If the node is a predefined function or operator from %SBML
 * Level&nbsp;1 (in the string-based formula syntax used in Level&nbsp;1) or
 * %SBML Level&nbsp;2 and&nbsp;3 (in the subset of MathML used in SBML
 * Levels&nbsp;2 and&nbsp;3), then the node's type
 * will be either <code style="margin-right: 0">AST_FUNCTION_</code><span
 * class="placeholder-nospace">X</span>, <code style="margin-right: 0">AST_LOGICAL_</code><span
 * class="placeholder-nospace">X</span>, or <code style="margin-right: 0">AST_RELATIONAL_</code><span
 * class="placeholder-nospace">X</span>, as appropriate.  (Examples:
 * @sbmlconstant{AST_FUNCTION_LOG, ASTNodeType_t},
 * @sbmlconstant{AST_RELATIONAL_LEQ, ASTNodeType_t}.)
 *
 * @li If the node refers to a user-defined function, the node's type will
 * be @sbmlconstant{AST_FUNCTION, ASTNodeType_t} (because it holds the
 * name of the function).
 *
 * @li If the node is a lambda expression, its type will be
 * @sbmlconstant{AST_LAMBDA, ASTNodeType_t}.
 * 
 * @li If the node is a predefined constant (@c "ExponentialE", @c "Pi", @c
 * "True" or @c "False"), then the node's type will be
 * @sbmlconstant{AST_CONSTANT_E, ASTNodeType_t},
 * @sbmlconstant{AST_CONSTANT_PI, ASTNodeType_t},
 * @sbmlconstant{AST_CONSTANT_TRUE, ASTNodeType_t}, or
 * @sbmlconstant{AST_CONSTANT_FALSE, ASTNodeType_t}.
 * 
 * @li (Levels&nbsp;2 and&nbsp;3 only) If the node is the special MathML
 * csymbol @c time, the value of the node will be
 * @sbmlconstant{AST_NAME_TIME, ASTNodeType_t}.  (Note, however, that the
 * MathML csymbol @c delay is translated into a node of type
 * @sbmlconstant{AST_FUNCTION_DELAY, ASTNodeType_t}.  The difference is due to
 * the fact that @c time is a single variable, whereas @c delay is actually a
 * function taking arguments.)
 *
 * @li (Level&nbsp;3 only) If the node is the special MathML csymbol @c
 * avogadro, the value of the node will be
 * @sbmlconstant{AST_NAME_AVOGADRO, ASTNodeType_t}.
 *
 * @li If the node contains a numerical value, its type will be
 * @sbmlconstant{AST_INTEGER, ASTNodeType_t},
 * @sbmlconstant{AST_REAL, ASTNodeType_t},
 * @sbmlconstant{AST_REAL_E, ASTNodeType_t}, or
 * @sbmlconstant{AST_RATIONAL, ASTNodeType_t}, as appropriate.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_summary_of_string_math
 *
 * @par
 * The text-string form of mathematical formulas produced by
 * @sbmlfunction{formulaToString, ASTNode tree} and read by
 * @sbmlfunction{parseFormula, String formula} use a simple C-inspired infix
 * notation taken from SBML Level&nbsp;1.  A formula in this text-string form
 * therefore can be handed to a program that understands SBML Level&nbsp;1
 * mathematical expressions, or used as part of a formula translation system.
 * The syntax is described in detail in the documentation for ASTNode.  The
 * following are illustrative examples of formulas expressed using this syntax:
 * @verbatim
0.10 * k4^2
@endverbatim
@verbatim
(vm * s1)/(km + s1)
@endverbatim
 *
 * Note that this facility is provided as a convenience by libSBML---the
 * MathML standard does not actually define a "string-form" equivalent to
 * MathML expression trees, so the choice of formula syntax is somewhat
 * arbitrary.  The approach taken by libSBML is to use the syntax defined by
 * SBML Level&nbsp;1 (which in fact used a text-string representation of
 * formulas and not MathML).  This formula syntax is based mostly on C
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
 * operations is determined; for example, <code>a - b + c</code> is
 * evaluated as <code>(a - b) + c</code> because the @c + and @c -
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
 * Table&nbsp;6 in the <a target="_blank" href="http://sbml.org/Documents/Specifications#SBML_Level_1_Version_2">SBML Level&nbsp;1 Version&nbsp;2 specification</a>:
 *
 * @htmlinclude string-functions-table.html
 *
 * @warning <span class="warning">There are differences between the symbols
 * used to represent the common mathematical functions and the corresponding
 * MathML token names.  This is a potential source of incompatibilities.
 * Note in particular that in this text-string syntax, <code>log(x)</code>
 * represents the natural logarithm, whereas in MathML, the natural logarithm
 * is <code>&lt;ln/&gt;</code>.  Application writers are urged to be careful
 * when translating between text forms and MathML forms, especially if they
 * provide a direct text-string input facility to users of their software
 * systems.</span>
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_summary_of_string_math_l3
 *
 * @par
 * The text-string form of mathematical formulas read by the function
 * @sbmlfunction{parseL3Formula, String formula} and written by the function
 * @sbmlfunction{formulaToL3String, ASTNode tree} uses an expanded version of
 * the syntax read and written by @sbmlfunction{parseFormula, String formula}
 * and @sbmlfunction{formulaToString, ASTNode tree}, respectively.  The
 * latter two libSBML functions were originally developed to support
 * conversion between SBML Levels&nbsp;1 and&nbsp;2, and were focused on the
 * syntax of mathematical formulas used in SBML Level&nbsp;1.  With time, and
 * the use of MathML in SBML Levels&nbsp;2 and&nbsp;3, it became clear that
 * supporting Level&nbsp;2 and&nbsp;3's expanded mathematical syntax would be
 * useful for software developers.  To maintain backwards compatibility for
 * libSBML users, the original @sbmlfunction{formulaToString, ASTNode tree}
 * and @sbmlfunction{parseFormula, String formula} have been left untouched,
 * and instead, the new functionality is provided in the form of
 * @sbmlfunction{parseL3Formula, String formula} and
 * @sbmlfunction{formulaToL3String, ASTNode tree}.
 *
 * The following lists the main differences in the formula syntax supported by
 * the "Level 3" or L3 versions of the formula parsers and formatters,
 * compared to what is supported by the Level&nbsp;1-oriented
 * @sbmlfunction{parseFormula, String formula} and
 * @sbmlfunction{formulaToString, ASTNode node}:
 *
 * @li Units may be asociated with bare numbers, using the following syntax:
 * <div style="margin: 10px auto 10px 25px; display: block">
 * <span class="code" style="background-color: #d0d0ee">number</span>
 * <span class="code" style="background-color: #edd">unit</span>
 * </div>
 * The <span class="code" style="background-color: #d0d0ee">number</span>
 * may be in any form (an integer, real, or rational
 * number), and the 
 * <span class="code" style="background-color: #edd">unit</span>
 * must conform to the syntax of an SBML identifier (technically, the
 * type defined as @c SId in the SBML specifications).  The whitespace between
 * <span class="code" style="background-color: #d0d0ee">number</span>
 * and <span class="code" style="background-color: #edd">unit</span>
 * is optional.
 *
 * @li The Boolean function symbols @c &&, @c ||, @c !, and @c != may be
 * used.
 *
 * @li The @em modulo operation is allowed as the symbol @c @% and will
 * produce a <code>&lt;piecewise&gt;</code> function in the corresponding
 * MathML output.
 *
 * @li All inverse trigonometric functions may be defined in the infix either
 * using @c arc as a prefix or simply @c a; in other words, both @c arccsc
 * and @c acsc are interpreted as the operator @em arccosecant as defined in
 * MathML&nbsp;2.0.  (Many functions in the simpler SBML Level&nbsp;1
 * oriented parser implemented by @sbmlfunction{parseFormula, String formula}
 * are defined this way as well, but not all.)
 *
 * @li The following expression is parsed as a rational number instead of
 * as a numerical division:
 * <pre style="display: block; margin-left: 25px">
 * (<span class="code" style="background-color: #d0d0ee">integer</span>/<span class="code" style="background-color: #d0d0ee">integer</span>)</pre>
 * <strong>Spaces are not allowed</strong> in this construct; in other words,
 * &quot;<code>(3 / 4)</code>&quot; (with whitespace between the numbers and
 * the operator) will be parsed into the MathML <code>&lt;divide&gt;</code>
 * construct rather than a rational number.  You can, however, assign units to a
 * rational number as a whole; here is an example: &quot;<code>(3/4) ml</code>&quot;.
 * (In the case of division rather than a rational number, units are not interpreted
 * in this way.)
 *
 * @li Various parser and formatter behaviors may be altered through the use
 * of a L3ParserSettings object in conjunction with the functions
 * @sbmlfunction{parseL3FormulaWithSettings, String formula\,
 * L3ParserSettings settings} and
 * @sbmlfunction{formulaToL3StringWithSettings, ASTNode tree\,
 * L3ParserSettings settings}
 * The settings available include the following:
 * <ul style="list-style-type: circle">
 *
 * <li style="margin-bottom: 0.5em"> The function @c log with a single
 * argument (&quot;<code>log(x)</code>&quot;) can be parsed as
 * <code>log10(x)</code>, <code>ln(x)</code>, or treated as an error, as
 * desired.
 *
 * <li style="margin-bottom: 0.5em"> Unary minus signs can be collapsed or
 * preserved; that is, sequential pairs of unary minuses (e.g., &quot;<code>-
 * -3</code>&quot;) can be removed from the input entirely and single unary
 * minuses can be incorporated into the number node, or all minuses can be
 * preserved in the AST node structure.
 *
 * <li style="margin-bottom: 0.5em"> Parsing of units embedded in the input
 * string can be turned on and off.
 *
 * <li style="margin-bottom: 0.5em"> The string @c avogadro can be parsed as
 * a MathML @em csymbol or as an identifier.
 *
 * <li style="margin-bottom: 0.5em"> A Model object may optionally be
 * provided to the parser using the variant function call
 * @sbmlfunction{parseL3FormulaWithModel, String formula\, Model model} or
 * stored in a L3ParserSettings object passed to the variant function
 * @sbmlfunction{parseL3FormulaWithSettings, String formula\,
 * L3ParserSettings settings}.  When a Model object is provided, identifiers
 * (values of type @c SId) from that model are used in preference to
 * pre-defined MathML definitions for both symbols and functions.
 * More precisely:
 * <ul style="list-style-type: square">
 *
 * <li style="margin-bottom: 0.5em"> <em>In the case of symbols</em>: the
 * Model entities whose identifiers will shadow identical symbols in the
 * mathematical formula are: Species, Compartment, Parameter, Reaction, and
 * SpeciesReference.  For instance, if the parser is given a Model containing
 * a Species with the identifier &quot;<code>pi</code>&quot;, and the formula
 * to be parsed is &quot;<code>3*pi</code>&quot;, the MathML produced will
 * contain the construct <code>&lt;ci&gt; pi &lt;/ci&gt;</code> instead of
 * the construct <code>&lt;pi/&gt;</code>.
 *
 * <li style="margin-bottom: 0.5em"> <em>In the case of user-defined
 * functions</em>: when a Model object is provided, @c SId values of
 * user-defined functions present in the model will be used preferentially
 * over pre-defined MathML functions.  For example, if the passed-in Model
 * contains a FunctionDefinition object with the identifier
 * &quot;<code>sin</code>&quot;, that function will be used instead of the
 * predefined MathML function <code>&lt;sin/&gt;</code>.
 * </ul>
 *
 * <li style="margin-bottom: 0.5em"> An SBMLNamespaces object may optionally
 * be provided to identify SBML Level&nbsp;3 packages that extend the
 * syntax understood by the formula parser.  When the namespaces are provided,
 * the parser will interpret possible additional syntax defined by the libSBML
 * plug-ins implementing the SBML Level&nbsp;3 packages; for example, it may
 * understand vector/array extensions introduced by the SBML Level&nbsp;3 @em
 * Arrays package.
 * </ul>
 *
 * These configuration settings cannot be changed directly using the basic
 * parser and formatter functions, but @em can be changed on a per-call basis
 * by using the alternative functions @sbmlfunction{parseL3FormulaWithSettings,
 * String formula\, L3ParserSettings settings} and
 * @sbmlfunction{formulaToL3StringWithSettings, ASTNode tree\,
 * L3ParserSettings settings}.
 *
 * Neither SBML nor the MathML standard define a "string-form" equivalent to
 * MathML expressions.  The approach taken by libSBML is to start with the
 * formula syntax defined by SBML Level&nbsp;1 (which in fact used a custom
 * text-string representation of formulas, and not MathML), and expand it to
 * include the functionality described above.  This formula syntax is based
 * mostly on C programming syntax, and may contain operators, function calls,
 * symbols, and white space characters.  The following table provides the
 * precedence rules for the different entities that may appear in formula
 * strings.
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
 * characters), followed by a closing parenthesis token.  The function name
 * must be chosen from one of the pre-defined functions in SBML or a
 * user-defined function in the model.  The following table lists the names
 * of certain common mathematical functions; this table corresponds to
 * Table&nbsp;6 in the <a target="_blank"
 * href="http://sbml.org/Documents/Specifications#SBML_Level_1_Version_2">SBML
 * Level&nbsp;1 Version&nbsp;2 specification</a> with additions based on the
 * functions added in SBML Level 2 and Level 3:
 *
 * @htmlinclude string-functions-table-l3.html
 *
 * Parsing of the various MathML functions and constants are all
 * case-insensitive by default: function names such as <code>cos</code>,
 * <code>Cos</code> and <code>COS</code> are all parsed as the MathML cosine
 * operator, <code>&lt;cos&gt;</code>.  However, <em>when a Model object is
 * used</em> in conjunction with either
 * @sbmlfunction{parseL3FormulaWithModel, String formula\, Model model} or
 * @sbmlfunction{parseL3FormulaWithSettings, String formula\,
 * L3ParserSettings settings}, any identifiers found in that model will be
 * parsed in a case-<em>sensitive</em> way.  For example, if a model contains
 * a Species having the identifier <code>Pi</code>, the parser will parse
 * &quot;<code>Pi</code>&quot; in the input as &quot;<code>&lt;ci&gt; Pi
 * &lt;/ci&gt;</code>&quot; but will continue to parse the symbols
 * &quot;<code>pi</code>&quot; and &quot;<code>PI</code>&quot; as
 * &quot;<code>&lt;pi&gt;</code>&quot;.
 *
 * As mentioned above, the manner in which the "L3" versions of the formula
 * parser and formatter interpret the function &quot;<code>log</code>&quot;
 * can be changed.  To do so, callers should use the function
 * @sbmlfunction{parseL3FormulaWithSettings, String formula\,
 * L3ParserSettings settings} and pass it an appropriate L3ParserSettings
 * object.  By default, unlike the SBML Level&nbsp;1 parser implemented by
 * @sbmlfunction{parseFormula, String formula}, the string
 * &quot;<code>log</code>&quot; is interpreted as the base&nbsp;10 logarithm,
 * and @em not as the natural logarithm.  However, you can change the
 * interpretation to be base-10 log, natural log, or as an error; since the
 * name "log" by itself is ambiguous, you require that the parser uses @c
 * log10 or @c ln instead, which are more clear.  Please refer to
 * @sbmlfunction{parseL3FormulaWithSettings, String formula\,
 * L3ParserSettings settings}.
 *
 * In addition, the following symbols will be translated to their MathML
 * equivalents, if no symbol with the same @c SId identifier string exists
 * in the Model object provided:
 *
 * @htmlinclude string-values-table-l3.html
 *
 * Again, as mentioned above, whether the string
 * &quot;<code>avogadro</code>&quot; is parsed as an AST node of type
 * @sbmlconstant{AST_NAME_AVOGADRO, ASTNodeType_t} or
 * @sbmlconstant{AST_NAME, ASTNodeType_t} is configurable; use the version of
 * the parser function called @sbmlfunction{parseL3FormulaWithSettings,
 * String formula\, L3ParserSettings settings}.  This Avogadro-related
 * functionality is provided because SBML Level&nbsp;2 models may not use
 * @sbmlconstant{AST_NAME_AVOGADRO, ASTNodeType_t} AST nodes.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_l3_parser_configuration_options
 *
 * @li A Model object may optionally be provided to use identifiers (values
 * of type @c SId) from the model in preference to pre-defined MathML symbols
 * More precisely, the Model entities whose identifiers will shadow identical
 * symbols in the mathematical formula are: Species, Compartment, Parameter,
 * Reaction, and SpeciesReference.  For instance, if the parser is given a
 * Model containing a Species with the identifier
 * &quot;<code>pi</code>&quot;, and the formula to be parsed is
 * &quot;<code>3*pi</code>&quot;, the MathML produced by the parser will
 * contain the construct <code>&lt;ci&gt; pi &lt;/ci&gt;</code> instead of
 * the construct <code>&lt;pi/&gt;</code>.  Another example, if the passed-in
 * Model contains a FunctionDefinition with the identifier
 * &quot;<code>sin</code>&quot;, that function will be used instead of the
 * predefined MathML function <code>&lt;sin/&gt;</code>.
 * @li The function @c log with a single argument
 * (&quot;<code>log(x)</code>&quot;) can be parsed as <code>log10(x)</code>,
 * <code>ln(x)</code>, or treated as an error, as desired.
 * @li Unary minus signs can be either collapsed or preserved; that is, the
 * parser can either (1) remove sequential pairs of unary minuses (e.g.,
 * &quot;<code>- -3</code>&quot;) from the input and incorporate single unary
 * minuses into the number node, or (2) preserve all minuses in the AST node
 * structure, turning them into ASTNode objects of type
 * @sbmlconstant{AST_MINUS, ASTNodeType_t}.
 * @li The character sequence &quot;<code>number id</code>&quot; can be
 * interpreted as a numerical value @c number followed by units of measurement
 * indicated by @c id, or it can be treated as a syntax error.  (In
 * Level&nbsp;3, MathML <code>&lt;cn&gt;</code> elements can have an
 * attribute named @c units placed in the SBML namespace, which can be used
 * to indicate the units to be associated with the number.  The text-string
 * infix formula parser allows units to be placed after raw numbers; they are
 * interpreted as unit identifiers for units defined by the SBML
 * specification or in the containing Model object.)
 * @li The symbol @c avogadro can be parsed either as a MathML @em csymbol or
 * as a identifier.  More specifically, &quot;<code>avogadro</code>&quot; can
 * be treated as an ASTNode of type
 * @sbmlconstant{AST_NAME_AVOGADRO, ASTNodeType_t} or of type
 * @sbmlconstant{AST_NAME, ASTNodeType_t}.
 * @li LibSBML plug-ins implementing support for SBML Level&nbsp;3 packages
 * may introduce extensions to the syntax understood by the parser.  The
 * precise nature of the extensions will be documented by the individual
 * package plug-ins.  An example of a possible extension is a notation for
 * vectors and arrays, introduced by the SBML Level&nbsp;3 @em Arrays
 * package.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_note_l3_parser_encouraged
 *
 * @note
 * Callers using SBML Level&nbsp;3 are encouraged to use the facilities
 * provided by libSBML's newer and more powerful Level&nbsp;3-oriented
 * formula parser and formatter.  The entry points to this second system are
 * @sbmlfunction{parseL3Formula, String formula} and
 * @sbmlfunction{formulaToL3String, ASTNode tree}.  The Level&nbsp;1-oriented
 * system (i.e., what is provided by @sbmlfunction{formulaToString, String
 * formula} and @sbmlfunction{parseFormula, ASTNode tree}) is provided
 * untouched for backwards compatibility.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_config_for_reading_zipped_files
 *
 * @par
 * To read a gzip/zip file, libSBML needs to be configured and linked with the
 * <a target="_blank" href="http://www.zlib.net/">zlib</a> library at compile
 * time.  It also needs to be linked with the <a target="_blank"
 * href="">bzip2</a> library to read files in <em>bzip2</em> format.  (Both of
 * these are the default configurations for libSBML.)  Errors about unreadable
 * files will be logged if a compressed filename is given and libSBML was
 * <em>not</em> linked with the corresponding required library.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_config_for_writing_zipped_files
 *
 * @par
 * To write a gzip/zip file, libSBML needs to be configured and linked with
 * the <a target="_blank" href="http://www.zlib.net/">zlib</a> library at
 * compile time.  It also needs to be linked with the <a target="_blank"
 * href="">bzip2</a> library to write files in <em>bzip2</em> format.  (Both
 * of these are the default configurations for libSBML.)  Errors about
 * unreadable files will be logged and this method will return
 * <code>false</code> if a compressed filename is given and libSBML was
 * <em>not</em> linked with the corresponding required library.
 * 
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_rules_general_summary
 *
 * @section rules-general General summary of SBML rules
 *
 * In SBML Level&nbsp;3 as well as Level&nbsp;2, rules are separated into three
 * subclasses for the benefit of model analysis software.  The three
 * subclasses are based on the following three different possible functional
 * forms (where <em>x</em> is a variable, <em>f</em> is some arbitrary
 * function returning a numerical result, <b><em>V</em></b> is a vector of
 * variables that does not include <em>x</em>, and <b><em>W</em></b> is a
 * vector of variables that may include <em>x</em>):
 * 
 * <table border="0" cellpadding="0" class="centered" style="font-size: small">
 * <tr><td width="120px"><em>Algebraic:</em></td><td width="250px">left-hand side is zero</td><td><em>0 = f(<b>W</b>)</em></td></tr>
 * <tr><td><em>Assignment:</em></td><td>left-hand side is a scalar:</td><td><em>x = f(<b>V</b>)</em></td></tr>
 * <tr><td><em>Rate:</em></td><td>left-hand side is a rate-of-change:</td><td><em>dx/dt = f(<b>W</b>)</em></td></tr>
 * </table>
 * 
 * In their general form given above, there is little to distinguish
 * between <em>assignment</em> and <em>algebraic</em> rules.  They are treated as
 * separate cases for the following reasons:
 * 
 * @li <em>Assignment</em> rules can simply be evaluated to calculate
 * intermediate values for use in numerical methods.  They are statements
 * of equality that hold at all times.  (For assignments that are only
 * performed once, see InitialAssignment.)

 * @li SBML needs to place restrictions on assignment rules, for example
 * the restriction that assignment rules cannot contain algebraic loops.
 * 
 * @li Some simulators do not contain numerical solvers capable of solving
 * unconstrained algebraic equations, and providing more direct forms such
 * as assignment rules may enable those simulators to process models they
 * could not process if the same assignments were put in the form of
 * general algebraic equations;
 * 
 * @li Those simulators that <em>can</em> solve these algebraic equations make a
 * distinction between the different categories listed above; and
 * 
 * @li Some specialized numerical analyses of models may only be applicable
 * to models that do not contain <em>algebraic</em> rules.
 * 
 * The approach taken to covering these cases in SBML is to define an
 * abstract Rule structure containing a subelement, "math", to hold the
 * right-hand side expression, then to derive subtypes of Rule that add
 * attributes to distinguish the cases of algebraic, assignment and rate
 * rules.  The "math" subelement must contain a MathML expression defining the
 * mathematical formula of the rule.  This MathML formula must return a
 * numerical value.  The formula can be an arbitrary expression referencing
 * the variables and other entities in an SBML model.
 * 
 * Each of the three subclasses of Rule (AssignmentRule, AlgebraicRule,
 * RateRule) inherit the the "math" subelement and other fields from SBase.
 * The AssignmentRule and RateRule classes add an additional attribute,
 * "variable".  See the definitions of AssignmentRule, AlgebraicRule and
 * RateRule for details about the structure and interpretation of each one.
 * 
 * @section rules-restrictions Additional restrictions on SBML rules
 *
 * An important design goal of SBML rule semantics is to ensure that a
 * model's simulation and analysis results will not be dependent on when or
 * how often rules are evaluated.  To achieve this, SBML needs to place two
 * restrictions on rule use.  The first concerns algebraic loops in the system
 * of assignments in a model, and the second concerns overdetermined systems.
 * 
 * @subsection rules-no-loops A model must not contain algebraic loops
 * 
 * The combined set of InitialAssignment, AssignmentRule and KineticLaw
 * objects in a model constitute a set of assignment statements that should be
 * considered as a whole.  (A KineticLaw object is counted as an assignment
 * because it assigns a value to the symbol contained in the "id" attribute of
 * the Reaction object in which it is defined.)  This combined set of
 * assignment statements must not contain algebraic loops---dependency
 * chains between these statements must terminate.  To put this more formally,
 * consider a directed graph in which nodes are assignment statements and
 * directed arcs exist for each occurrence of an SBML species, compartment or
 * parameter symbol in an assignment statement's "math" subelement.  Let the
 * directed arcs point from the statement assigning the symbol to the
 * statements that contain the symbol in their "math" subelement expressions.
 * This graph must be acyclic.
 * 
 * SBML does not specify when or how often rules should be evaluated.
 * Eliminating algebraic loops ensures that assignment statements can be
 * evaluated any number of times without the result of those evaluations
 * changing.  As an example, consider the set of equations <em>x = x + 1</em>,
 * <em>y = z + 200</em> and <em>z = y + 100</em>.  If this set of equations
 * were interpreted as a set of assignment statements, it would be invalid
 * because the rule for <em>x</em> refers to <em>x</em> (exhibiting one type
 * of loop), and the rule for <em>y</em> refers to <em>z</em> while the rule
 * for <em>z</em> refers back to <em>y</em> (exhibiting another type of loop).
 * Conversely, the following set of equations would constitute a valid set of
 * assignment statements: <em>x = 10</em>, <em>y = z + 200</em>, and <em>z = x
 * + 100</em>.
 * 
 * @subsection rules-not-overdetermined A model must not be overdetermined
 * 
 * An SBML model must not be overdetermined; that is, a model must not
 * define more equations than there are unknowns in a model.  An SBML model
 * that does not contain AlgebraicRule structures cannot be overdetermined.
 * 
 * LibSBML implements the static analysis procedure described in
 * Appendix&nbsp;B of the SBML Level&nbsp;3 Version&nbsp;1 Core
 * specification for assessing whether a model is overdetermined.
 * 
 * (In summary, assessing whether a given continuous, deterministic,
 * mathematical model is overdetermined does not require dynamic analysis; it
 * can be done by analyzing the system of equations created from the model.
 * One approach is to construct a bipartite graph in which one set of vertices
 * represents the variables and the other the set of vertices represents the
 * equations.  Place edges between vertices such that variables in the system
 * are linked to the equations that determine them.  For algebraic equations,
 * there will be edges between the equation and each variable occurring in the
 * equation.  For ordinary differential equations (such as those defined by
 * rate rules or implied by the reaction rate definitions), there will be a
 * single edge between the equation and the variable determined by that
 * differential equation.  A mathematical model is overdetermined if the
 * maximal matchings of the bipartite graph contain disconnected vertexes
 * representing equations.  If one maximal matching has this property, then
 * all the maximal matchings will have this property; i.e., it is only
 * necessary to find one maximal matching.)
 *
 * @section RuleType_t Rule types for SBML Level 1
 *
 * SBML Level 1 uses a different scheme than SBML Level 2 and Level 3 for
 * distinguishing rules; specifically, it uses an attribute whose value is
 * drawn from an enumeration of 3 values.  LibSBML supports this using methods
 * that work @if clike a libSBML enumeration type, RuleType_t, whose values
 * are @else with the enumeration values @endif@~ listed below.
 *
 * @li @sbmlconstant{RULE_TYPE_RATE, RuleType_t}: Indicates
 * the rule is a "rate" rule.
 * @li @sbmlconstant{RULE_TYPE_SCALAR, RuleType_t}:
 * Indicates the rule is a "scalar" rule.
 * @li @sbmlconstant{RULE_TYPE_INVALID, RuleType_t}:
 * Indicates the rule type is unknown or not yet set.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_note_unassigned_unit_are_not_a_default
 *
 * @note There is an important distinction to be made between <em>no units
 * assigned</em>, and assuming a value without units has any specific unit
 * such as <code>dimensionless</code>.  In SBML, default units are never
 * attributed to numbers, and numbers without units are not automatically
 * assumed to have the unit <code>dimensionless</code>.  Please consult the
 * relevant SBML specification document for a more in-depth explanation of
 * this topic and the SBML unit system.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_note_unit_inference_depends_on_model
 * 
 * @note The functionality that facilitates unit analysis depends on the
 * model as a whole.  Thus, in cases where the object has not been added to
 * a model or the model itself is incomplete, unit analysis is not possible
 * and this method will return @c NULL.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_note_object_is_copied
 * 
 * @note This method should be used with some caution.  The fact that this
 * method @em copies the object passed to it means that the caller will be
 * left holding a physically different object instance than the one contained
 * inside this object.  Changes made to the original object instance (such as
 * resetting attribute values) will <em>not affect the instance in this
 * object</em>.  In addition, the caller should make sure to free the
 * original object if it is no longer being used, or else a memory leak will
 * result.  Please see other methods on this class (particularly a
 * corresponding method whose name begins with the word <code>create</code>)
 * for alternatives that do not lead to these issues.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_note_math_string_syntax
 *
 * @note We urge developers to keep in mind that the text-string formula
 * syntax is specific to libSBML.  <em>Neither MathML nor SBML define a
 * text-string format for mathematical formulas.</em> LibSBML's particular
 * syntax should not be considered to be a canonical or standard
 * general-purpose mathematical expression syntax.  LibSBML provides methods
 * for parsing and transforming text-string math formulas back and forth from
 * AST structures for the convenience of calling applications, but it is
 * important to keep the system's limitations in mind.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_additional_typecode_details
 *
 * @par
 * Here follow some additional general information about SBML type codes:
 *
 * @li The codes are the possible return values (integers) for the following
 * functions:
 * <ul>
 *     <li> virtual int SBase::getTypeCode() const;
 *     <li> virtual int ListOf::getItemTypeCode() const;
 * </ul>
 * (In libSBML 5, the type of return values of these functions changed from
 * an enumeration to an integer for extensibility in the face of different
 * programming languages.)
 *
 * @li Each package extension must define similar sets of values for each
 * SBase subclass (e.g. #SBMLLayoutTypeCode_t for the SBML Level&nbsp;3
 * Layout extension, #SBMLFbcTypeCode_t for the SBML Level&nbsp;3 Flux
 * Balance Constraints extension, etc.).
 *
 * @li The value of each package-specific type code can be duplicated between
 * those of different packages.  (This is necessary because the development
 * of libSBML extensions for different SBML packages may be undertaken by
 * different developers at different times; requiring the developers to
 * coordinate their use of type codes would be nettlesome and probably
 * doomed to failure.)
 *
 * @li To distinguish between the type codes of different packages, both the
 * return value of SBase::getTypeCode() and SBase::getPackageName() must be
 * checked.  This is particularly important for functions that take an SBML
 * type code as an argument, such as SBase::getAncestorOfType(), which by
 * default assumes you are handing it a core type, and will return @c NULL if
 * the value you give it is actually from a package.
 *
 * The following example code illustrates the combined use of
 * SBase::getPackageName() and SBase::getTypeCode():
 *@verbatim
 void example (const SBase *sb)
 {
   cons std::string pkgName = sb->getPackageName();
   if (pkgName == "core")
   {
     switch (sb->getTypeCode())
     {
       case SBML_MODEL:
          ....
          break;
       case SBML_REACTION:
          ....
     }
   } 
   else if (pkgName == "layout")
   {
     switch (sb->getTypeCode())
     {
       case SBML_LAYOUT_LAYOUT:
          ....
          break;
       case SBML_LAYOUT_REACTIONGLYPH:
          ....
     }
   } 
   ...
 } 
 @endverbatim
 * 
 *  @see #SBMLTypeCode_t
 *  @see #SBMLCompTypeCode_t
 *  @see #SBMLFbcTypeCode_t
 *  @see #SBMLLayoutTypeCode_t
 *  @see #SBMLQualTypeCode_t
 * 
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_sbml_error_code_ranges
 * 
 * @par
 * Calling programs may wish to check which enumeration a given SBMLError
 * object's error identifier is actually from:
 * @li 0000000 to 0009999: #XMLErrorCode_t (a low-level XML problem)
 *
 * @li 0010000 to 0099999: #SBMLErrorCode_t (a problem with the SBML
 * core specification)
 * @li 1000000 to 1099999: #CompSBMLErrorCode_t (a problem with the SBML
 * Level&nbsp;3 Hierarchical %Model Composition package specification).
 *
 * @li 2000000 to 2099999: #FbcSBMLErrorCode_t (a problem with the SBML
 * Level&nbsp;3 Flux Balance Constraints package specification).
 * 
 * @li 3000000 to 3099999: #QualSBMLErrorCode_t (a problem with the SBML
 * Level&nbsp;3 Qualitative Models package specification).
 * 
 * @li 6000000 to 6099999: #LayoutSBMLErrorCode_t (a problem with the SBML
 * Level&nbsp;3 %Layout package specification).
 *
 * Other error code ranges are reserved for other packages.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_sbml_error_table
 *
 * @par
<table id="sbmlerror-table"
       class="text-table small-font alt-row-colors"
       width="95%" cellspacing="1" cellpadding="2" border="0">
 <tr style="background: lightgray" class="normal-font">
     <th valign="bottom"><strong>Enumerator</strong></th>
     <th valign="bottom"><strong>Meaning</strong></th>
     <th align="center" width="10">L1 V1</th>
     <th align="center" width="10">L1 V2</th>
     <th align="center" width="10">L2 V1</th>
     <th align="center" width="10">L2 V2</th>
     <th align="center" width="10">L2 V3</th>
     <th align="center" width="10">L2 V4</th>
     <th align="center" width="10">L3 V1</th>
 </tr>
<tr><td class="code">@sbmlconstant{XMLUnknownError, SBMLErrorCode_t}</td>
<td class="meaning">Unknown error</td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLOutOfMemory, SBMLErrorCode_t}</td>
<td class="meaning">Out of memory</td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLFileUnreadable, SBMLErrorCode_t}</td>
<td class="meaning">File unreadable</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLFileUnwritable, SBMLErrorCode_t}</td>
<td class="meaning">File unwritable</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLFileOperationError, SBMLErrorCode_t}</td>
<td class="meaning">File operation error</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLNetworkAccessError, SBMLErrorCode_t}</td>
<td class="meaning">Network access error</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InternalXMLParserError, SBMLErrorCode_t}</td>
<td class="meaning">Internal XML parser error</td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
</tr>
<tr><td class="code">@sbmlconstant{UnrecognizedXMLParserCode, SBMLErrorCode_t}</td>
<td class="meaning">Unrecognized XML parser code</td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLTranscoderError, SBMLErrorCode_t}</td>
<td class="meaning">Transcoder error</td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
</tr>
<tr><td class="code">@sbmlconstant{MissingXMLDecl, SBMLErrorCode_t}</td>
<td class="meaning">Missing XML declaration</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{MissingXMLEncoding, SBMLErrorCode_t}</td>
<td class="meaning">Missing XML encoding attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadXMLDecl, SBMLErrorCode_t}</td>
<td class="meaning">Bad XML declaration</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadXMLDOCTYPE, SBMLErrorCode_t}</td>
<td class="meaning">Bad XML DOCTYPE</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidCharInXML, SBMLErrorCode_t}</td>
<td class="meaning">Invalid character</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadlyFormedXML, SBMLErrorCode_t}</td>
<td class="meaning">Badly formed XML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{UnclosedXMLToken, SBMLErrorCode_t}</td>
<td class="meaning">Unclosed token</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidXMLConstruct, SBMLErrorCode_t}</td>
<td class="meaning">Invalid XML construct</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLTagMismatch, SBMLErrorCode_t}</td>
<td class="meaning">XML tag mismatch</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{DuplicateXMLAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Duplicate attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{UndefinedXMLEntity, SBMLErrorCode_t}</td>
<td class="meaning">Undefined XML entity</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadProcessingInstruction, SBMLErrorCode_t}</td>
<td class="meaning">Bad XML processing instruction</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadXMLPrefix, SBMLErrorCode_t}</td>
<td class="meaning">Bad XML prefix</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadXMLPrefixValue, SBMLErrorCode_t}</td>
<td class="meaning">Bad XML prefix value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{MissingXMLRequiredAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Missing required attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLAttributeTypeMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Attribute type mismatch</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLBadUTF8Content, SBMLErrorCode_t}</td>
<td class="meaning">Bad UTF8 content</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{MissingXMLAttributeValue, SBMLErrorCode_t}</td>
<td class="meaning">Missing attribute value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadXMLAttributeValue, SBMLErrorCode_t}</td>
<td class="meaning">Bad attribute value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadXMLAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Bad XML attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{UnrecognizedXMLElement, SBMLErrorCode_t}</td>
<td class="meaning">Unrecognized XML element</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadXMLComment, SBMLErrorCode_t}</td>
<td class="meaning">Bad XML comment</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadXMLDeclLocation, SBMLErrorCode_t}</td>
<td class="meaning">Bad XML declaration location</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLUnexpectedEOF, SBMLErrorCode_t}</td>
<td class="meaning">Unexpected EOF</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadXMLIDValue, SBMLErrorCode_t}</td>
<td class="meaning">Bad XML ID value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadXMLIDRef, SBMLErrorCode_t}</td>
<td class="meaning">Bad XML IDREF</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{UninterpretableXMLContent, SBMLErrorCode_t}</td>
<td class="meaning">Uninterpretable XML content</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadXMLDocumentStructure, SBMLErrorCode_t}</td>
<td class="meaning">Bad XML document structure</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidAfterXMLContent, SBMLErrorCode_t}</td>
<td class="meaning">Invalid content after XML content</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLExpectedQuotedString, SBMLErrorCode_t}</td>
<td class="meaning">Expected quoted string</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLEmptyValueNotPermitted, SBMLErrorCode_t}</td>
<td class="meaning">Empty value not permitted</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLBadNumber, SBMLErrorCode_t}</td>
<td class="meaning">Bad number</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLBadColon, SBMLErrorCode_t}</td>
<td class="meaning">Colon character not permitted</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{MissingXMLElements, SBMLErrorCode_t}</td>
<td class="meaning">Missing XML elements</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{XMLContentEmpty, SBMLErrorCode_t}</td>
<td class="meaning">Empty XML content</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{UnknownError, SBMLErrorCode_t}</td>
<td class="meaning">Encountered unknown internal libSBML error</td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
</tr>
<tr><td class="code">@sbmlconstant{NotUTF8, SBMLErrorCode_t}</td>
<td class="meaning">File does not use UTF-8 encoding</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{UnrecognizedElement, SBMLErrorCode_t}</td>
<td class="meaning">Encountered unrecognized element</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NotSchemaConformant, SBMLErrorCode_t}</td>
<td class="meaning">Document does not conform to the SBML XML schema</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{L3NotSchemaConformant, SBMLErrorCode_t}</td>
<td class="meaning">Document is not well-formed XML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidMathElement, SBMLErrorCode_t}</td>
<td class="meaning">Invalid MathML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{DisallowedMathMLSymbol, SBMLErrorCode_t}</td>
<td class="meaning">Disallowed MathML symbol found</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{DisallowedMathMLEncodingUse, SBMLErrorCode_t}</td>
<td class="meaning">Use of the MathML 'encoding' attribute is not allowed on this element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{DisallowedDefinitionURLUse, SBMLErrorCode_t}</td>
<td class="meaning">Use of the MathML 'definitionURL' attribute is not allowed on this element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadCsymbolDefinitionURLValue, SBMLErrorCode_t}</td>
<td class="meaning">Invalid <code>&lt;csymbol&gt;</code> 'definitionURL' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{DisallowedMathTypeAttributeUse, SBMLErrorCode_t}</td>
<td class="meaning">Use of the MathML 'type' attribute is not allowed on this element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{DisallowedMathTypeAttributeValue, SBMLErrorCode_t}</td>
<td class="meaning">Disallowed MathML 'type' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LambdaOnlyAllowedInFunctionDef, SBMLErrorCode_t}</td>
<td class="meaning">Use of <code>&lt;lambda&gt;</code> not permitted outside of FunctionDefinition objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BooleanOpsNeedBooleanArgs, SBMLErrorCode_t}</td>
<td class="meaning">Non-Boolean argument given to Boolean operator</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NumericOpsNeedNumericArgs, SBMLErrorCode_t}</td>
<td class="meaning">Non-numerical argument given to numerical operator</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{ArgsToEqNeedSameType, SBMLErrorCode_t}</td>
<td class="meaning">Arguments to <code>&lt;eq&gt;</code> and <code>&lt;neq&gt;</code> must have the same data types</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{PiecewiseNeedsConsistentTypes, SBMLErrorCode_t}</td>
<td class="meaning">Terms in a <code>&lt;piecewise&gt;</code> expression must have consistent data types</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{PieceNeedsBoolean, SBMLErrorCode_t}</td>
<td class="meaning">The second argument of a <code>&lt;piece&gt;</code> expression must yield a Boolean value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{ApplyCiMustBeUserFunction, SBMLErrorCode_t}</td>
<td class="meaning">A <code>&lt;ci&gt;</code> element in this context must refer to a function definition</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{ApplyCiMustBeModelComponent, SBMLErrorCode_t}</td>
<td class="meaning">A <code>&lt;ci&gt;</code> element in this context must refer to a model component</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{KineticLawParametersAreLocalOnly, SBMLErrorCode_t}</td>
<td class="meaning">Cannot use a KineticLaw local parameter outside of its local scope</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{MathResultMustBeNumeric, SBMLErrorCode_t}</td>
<td class="meaning">A formula's result in this context must be a numerical value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OpsNeedCorrectNumberOfArgs, SBMLErrorCode_t}</td>
<td class="meaning">Incorrect number of arguments given to MathML operator</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidNoArgsPassedToFunctionDef, SBMLErrorCode_t}</td>
<td class="meaning">Incorrect number of arguments given to function invocation</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{DisallowedMathUnitsUse, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'units' is only permitted on <code>&lt;cn&gt;</code> elements</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidUnitsValue, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value given for the 'units' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{DuplicateComponentId, SBMLErrorCode_t}</td>
<td class="meaning">Duplicate 'id' attribute value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{DuplicateUnitDefinitionId, SBMLErrorCode_t}</td>
<td class="meaning">Duplicate unit definition 'id' attribute value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{DuplicateLocalParameterId, SBMLErrorCode_t}</td>
<td class="meaning">Duplicate local parameter 'id' attribute value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{MultipleAssignmentOrRateRules, SBMLErrorCode_t}</td>
<td class="meaning">Multiple rules for the same variable are not allowed</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{MultipleEventAssignmentsForId, SBMLErrorCode_t}</td>
<td class="meaning">Multiple event assignments for the same variable are not allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{EventAndAssignmentRuleForId, SBMLErrorCode_t}</td>
<td class="meaning">An event assignment and an assignment rule must not have the same value for 'variable'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{DuplicateMetaId, SBMLErrorCode_t}</td>
<td class="meaning">Duplicate 'metaid' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidSBOTermSyntax, SBMLErrorCode_t}</td>
<td class="meaning">Invalid syntax for an 'sboTerm' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidMetaidSyntax, SBMLErrorCode_t}</td>
<td class="meaning">Invalid syntax for a 'metaid' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidIdSyntax, SBMLErrorCode_t}</td>
<td class="meaning">Invalid syntax for an 'id' attribute value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidUnitIdSyntax, SBMLErrorCode_t}</td>
<td class="meaning">Invalid syntax for the identifier of a unit</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidNameSyntax, SBMLErrorCode_t}</td>
<td class="meaning">Invalid syntax for a 'name' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{MissingAnnotationNamespace, SBMLErrorCode_t}</td>
<td class="meaning">Missing declaration of the XML namespace for the annotation</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{DuplicateAnnotationNamespaces, SBMLErrorCode_t}</td>
<td class="meaning">Multiple annotations using the same XML namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{SBMLNamespaceInAnnotation, SBMLErrorCode_t}</td>
<td class="meaning">The SBML XML namespace cannot be used in an Annotation object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{MultipleAnnotations, SBMLErrorCode_t}</td>
<td class="meaning">Only one Annotation object is permitted under a given SBML object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InconsistentArgUnits, SBMLErrorCode_t}</td>
<td class="meaning">The units of the function call's arguments are not consistent with its definition</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InconsistentKineticLawUnitsL3, SBMLErrorCode_t}</td>
<td class="meaning">The kinetic law's units are inconsistent with those of other kinetic laws in the model</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{AssignRuleCompartmentMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in assignment rule for compartment</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{AssignRuleSpeciesMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in assignment rule for species</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{AssignRuleParameterMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in assignment rule for parameter</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{AssignRuleStoichiometryMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in assignment rule for stoichiometry</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InitAssignCompartmenMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in initial assignment to compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InitAssignSpeciesMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in initial assignment to species</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InitAssignParameterMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in initial assignment to parameter</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InitAssignStoichiometryMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in initial assignment to stoichiometry</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{RateRuleCompartmentMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in rate rule for compartment</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{RateRuleSpeciesMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in rate rule for species</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{RateRuleParameterMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in rate rule for parameter</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{RateRuleStoichiometryMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in rate rule for stoichiometry</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{KineticLawNotSubstancePerTime, SBMLErrorCode_t}</td>
<td class="meaning">The units of the kinetic law are not 'substance'/'time'</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{SpeciesInvalidExtentUnits, SBMLErrorCode_t}</td>
<td class="meaning">The species' units are not consistent with units of extent</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{DelayUnitsNotTime, SBMLErrorCode_t}</td>
<td class="meaning">The units of the delay expression are not units of time</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{EventAssignCompartmentMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in event assignment for compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{EventAssignSpeciesMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in event assignment for species</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{EventAssignParameterMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in event assignment for parameter</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{EventAssignStoichiometryMismatch, SBMLErrorCode_t}</td>
<td class="meaning">Mismatched units in event assignment for stoichiometry</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{PriorityUnitsNotDimensionless, SBMLErrorCode_t}</td>
<td class="meaning">The units of a priority expression must be 'dimensionless'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{OverdeterminedSystem, SBMLErrorCode_t}</td>
<td class="meaning">The model is overdetermined</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidModelSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a Model object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidFunctionDefSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a FunctionDefinition object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidParameterSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a Parameter object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidInitAssignSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for an InitialAssignment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidRuleSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a Rule object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidConstraintSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a Constraint object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidReactionSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a Reaction object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidSpeciesReferenceSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a SpeciesReference object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidKineticLawSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a KineticLaw object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidEventSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for an Event object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidEventAssignmentSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for an EventAssignment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidCompartmentSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a Compartment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidSpeciesSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a Species object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidCompartmentTypeSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a CompartmentType object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidSpeciesTypeSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a SpeciesType object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidTriggerSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for an Event Trigger object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidDelaySBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'sboTerm' attribute value for an Event Delay object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{NotesNotInXHTMLNamespace, SBMLErrorCode_t}</td>
<td class="meaning">Notes must be placed in the XHTML XML namespace</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NotesContainsXMLDecl, SBMLErrorCode_t}</td>
<td class="meaning">XML declarations are not permitted in Notes objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NotesContainsDOCTYPE, SBMLErrorCode_t}</td>
<td class="meaning">XML DOCTYPE elements are not permitted in Notes objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidNotesContent, SBMLErrorCode_t}</td>
<td class="meaning">Invalid notes content found</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyOneNotesElementAllowed, SBMLErrorCode_t}</td>
<td class="meaning">Only one Notes subobject is permitted on a given SBML object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidNamespaceOnSBML, SBMLErrorCode_t}</td>
<td class="meaning">Invalid XML namespace for the SBML container element</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{MissingOrInconsistentLevel, SBMLErrorCode_t}</td>
<td class="meaning">Missing or inconsistent value for the 'level' attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{MissingOrInconsistentVersion, SBMLErrorCode_t}</td>
<td class="meaning">Missing or inconsistent value for the 'version' attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{PackageNSMustMatch, SBMLErrorCode_t}</td>
<td class="meaning">Inconsistent or invalid SBML Level/Version for the package namespace declaration</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LevelPositiveInteger, SBMLErrorCode_t}</td>
<td class="meaning">The 'level' attribute must have a positive integer value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{VersionPositiveInteger, SBMLErrorCode_t}</td>
<td class="meaning">The 'version' attribute must have a positive integer value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnSBML, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the SBML container element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{L3PackageOnLowerSBML, SBMLErrorCode_t}</td>
<td class="meaning">An L3 package ns found on the SBML container element</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{MissingModel, SBMLErrorCode_t}</td>
<td class="meaning">No model definition found</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{IncorrectOrderInModel, SBMLErrorCode_t}</td>
<td class="meaning">Incorrect ordering of components within the Model object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{EmptyListElement, SBMLErrorCode_t}</td>
<td class="meaning">Empty ListOf___ object found</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NeedCompartmentIfHaveSpecies, SBMLErrorCode_t}</td>
<td class="meaning">The presence of a species requires a compartment</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneOfEachListOf, SBMLErrorCode_t}</td>
<td class="meaning">Only one of each kind of ListOf___ object is allowed inside a Model object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyFuncDefsInListOfFuncDefs, SBMLErrorCode_t}</td>
<td class="meaning">Only FunctionDefinition, Notes and Annotation objects are allowed in ListOfFunctionDefinitions</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyUnitDefsInListOfUnitDefs, SBMLErrorCode_t}</td>
<td class="meaning">Only UnitDefinition, Notes and Annotation objects are allowed in ListOfUnitDefinitions objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyCompartmentsInListOfCompartments, SBMLErrorCode_t}</td>
<td class="meaning">Only Compartment, Notes and Annotation objects are allowed in ListOfCompartments objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlySpeciesInListOfSpecies, SBMLErrorCode_t}</td>
<td class="meaning">Only Species, Notes and Annotation objects are allowed in ListOfSpecies objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyParametersInListOfParameters, SBMLErrorCode_t}</td>
<td class="meaning">Only Parameter, Notes and Annotation objects are allowed in ListOfParameters objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyInitAssignsInListOfInitAssigns, SBMLErrorCode_t}</td>
<td class="meaning">Only InitialAssignment, Notes and Annotation objects are allowed in ListOfInitialAssignments objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyRulesInListOfRules, SBMLErrorCode_t}</td>
<td class="meaning">Only Rule, Notes and Annotation objects are allowed in ListOfRules objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyConstraintsInListOfConstraints, SBMLErrorCode_t}</td>
<td class="meaning">Only Constraint, Notes and Annotation objects are allowed in ListOfConstraints objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyReactionsInListOfReactions, SBMLErrorCode_t}</td>
<td class="meaning">Only Reaction, Notes and Annotation objects are allowed in ListOfReactions objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyEventsInListOfEvents, SBMLErrorCode_t}</td>
<td class="meaning">Only Event, Notes and Annotation objects are allowed in ListOfEvents objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{L3ConversionFactorOnModel, SBMLErrorCode_t}</td>
<td class="meaning">A 'conversionFactor' attribute value must reference a Parameter object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{L3TimeUnitsOnModel, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'timeUnits' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{L3VolumeUnitsOnModel, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'volumeUnits' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{L3AreaUnitsOnModel, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'areaUnits' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{L3LengthUnitsOnModel, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'lengthUnits' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{L3ExtentUnitsOnModel, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'extentUnits' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnModel, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the Model object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfFuncs, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfFunctionDefinitions object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfUnitDefs, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfUnitDefinitions object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfComps, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfCompartments object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfSpecies, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfSpecies object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfParams, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfParameters object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfInitAssign, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfInitialAssignments object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfRules, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfRules object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfConstraints, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfConstraints object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfReactions, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfReactions object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfEvents, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfEvents object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FunctionDefMathNotLambda, SBMLErrorCode_t}</td>
<td class="meaning">Invalid expression found in the function definition</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidApplyCiInLambda, SBMLErrorCode_t}</td>
<td class="meaning">Invalid forward reference in the MathML <code>&lt;apply&gt;</code><code>&lt;ci&gt;</code>...<code>&lt;/ci&gt;</code><code>&lt;/apply&gt;</code> expression</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{RecursiveFunctionDefinition, SBMLErrorCode_t}</td>
<td class="meaning">Recursive function definitions are not permitted</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidCiInLambda, SBMLErrorCode_t}</td>
<td class="meaning">Invalid <code>&lt;ci&gt;</code> reference found inside the <code>&lt;lambda&gt;</code> mathematical formula</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidFunctionDefReturnType, SBMLErrorCode_t}</td>
<td class="meaning">A function's return type must be either a number or a Boolean</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneMathElementPerFunc, SBMLErrorCode_t}</td>
<td class="meaning">A FunctionDefinition object must contain one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnFunc, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the FunctionDefinition object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidUnitDefId, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'id' attribute value for a UnitDefinition object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidSubstanceRedefinition, SBMLErrorCode_t}</td>
<td class="meaning">Invalid redefinition of built-in type 'substance'</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidLengthRedefinition, SBMLErrorCode_t}</td>
<td class="meaning">Invalid redefinition of built-in type 'length'</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidAreaRedefinition, SBMLErrorCode_t}</td>
<td class="meaning">Invalid redefinition of built-in type name 'area'</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidTimeRedefinition, SBMLErrorCode_t}</td>
<td class="meaning">Invalid redefinition of built-in type name 'time'</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidVolumeRedefinition, SBMLErrorCode_t}</td>
<td class="meaning">Invalid redefinition of built-in type name 'volume'</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{VolumeLitreDefExponentNotOne, SBMLErrorCode_t}</td>
<td class="meaning">Must use 'exponent'=1 when defining 'volume' in terms of litres</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{VolumeMetreDefExponentNot3, SBMLErrorCode_t}</td>
<td class="meaning">Must use 'exponent'=3 when defining 'volume' in terms of metres</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{EmptyListOfUnits, SBMLErrorCode_t}</td>
<td class="meaning">An empty list of Unit objects is not permitted in a UnitDefinition object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidUnitKind, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the 'kind' attribute of a UnitDefinition object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OffsetNoLongerValid, SBMLErrorCode_t}</td>
<td class="meaning">Unit attribute 'offset' is not supported in this Level+Version of SBML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{CelsiusNoLongerValid, SBMLErrorCode_t}</td>
<td class="meaning">Unit name 'Celsius' is not defined in this Level+Version of SBML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{EmptyUnitListElement, SBMLErrorCode_t}</td>
<td class="meaning">A ListOfUnits object must not be empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneListOfUnitsPerUnitDef, SBMLErrorCode_t}</td>
<td class="meaning">At most one ListOfUnits object is allowed inside a UnitDefinition object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyUnitsInListOfUnits, SBMLErrorCode_t}</td>
<td class="meaning">Only Unit, Notes and Annotation objects are allowed in ListOfUnits objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnUnitDefinition, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the UnitDefinition object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfUnits, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfUnits object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnUnit, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the Unit object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{ZeroDimensionalCompartmentSize, SBMLErrorCode_t}</td>
<td class="meaning">Invalid use of the 'size' attribute for a zero-dimensional compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{ZeroDimensionalCompartmentUnits, SBMLErrorCode_t}</td>
<td class="meaning">Invalid use of the 'units' attribute for a zero-dimensional compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{ZeroDimensionalCompartmentConst, SBMLErrorCode_t}</td>
<td class="meaning">Zero-dimensional compartments must be defined to be constant</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{UndefinedOutsideCompartment, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the 'outside' attribute of a Compartment object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{RecursiveCompartmentContainment, SBMLErrorCode_t}</td>
<td class="meaning">Recursive nesting of compartments via the 'outside' attribute is not permitted</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{ZeroDCompartmentContainment, SBMLErrorCode_t}</td>
<td class="meaning">Invalid nesting of zero-dimensional compartments</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{Invalid1DCompartmentUnits, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the 'units' attribute of a one-dimensional compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{Invalid2DCompartmentUnits, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the 'units' attribute of a two-dimensional compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{Invalid3DCompartmentUnits, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the 'units' attribute of a three-dimensional compartment</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidCompartmentTypeRef, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the 'compartmentType' attribute of a compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneDimensionalCompartmentUnits, SBMLErrorCode_t}</td>
<td class="meaning">No units defined for 1-D compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{TwoDimensionalCompartmentUnits, SBMLErrorCode_t}</td>
<td class="meaning">No units defined for 2-D compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{ThreeDimensionalCompartmentUnits, SBMLErrorCode_t}</td>
<td class="meaning">No units defined for 3-D Compartment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnCompartment, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on Compartment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoUnitsOnCompartment, SBMLErrorCode_t}</td>
<td class="meaning">No units defined for Compartment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidSpeciesCompartmentRef, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value found for Species 'compartment' attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{HasOnlySubsNoSpatialUnits, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'spatialSizeUnits' must not be set if 'hasOnlySubstanceUnits'='true'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoSpatialUnitsInZeroD, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'spatialSizeUnits' must not be set if the compartment is zero-dimensional</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoConcentrationInZeroD, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'initialConcentration' must not be set if the compartment is zero-dimensional</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{SpatialUnitsInOneD, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for 'spatialSizeUnits' attribute of a one-dimensional compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{SpatialUnitsInTwoD, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the 'spatialSizeUnits' attribute of a two-dimensional compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{SpatialUnitsInThreeD, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the 'spatialSizeUnits' attribute of a three-dimensional compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidSpeciesSusbstanceUnits, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for a Species 'units' attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{BothAmountAndConcentrationSet, SBMLErrorCode_t}</td>
<td class="meaning">Cannot set both 'initialConcentration' and 'initialAmount' attributes simultaneously</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NonBoundarySpeciesAssignedAndUsed, SBMLErrorCode_t}</td>
<td class="meaning">Cannot use a non-boundary species in both reactions and rules simultaneously</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NonConstantSpeciesUsed, SBMLErrorCode_t}</td>
<td class="meaning">Cannot use a constant, non-boundary species as a reactant or product</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidSpeciesTypeRef, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the 'speciesType' attribute of a species</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{MultSpeciesSameTypeInCompartment, SBMLErrorCode_t}</td>
<td class="meaning">Cannot have multiple species of the same species type in the same compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{MissingSpeciesCompartment, SBMLErrorCode_t}</td>
<td class="meaning">Missing value for the 'compartment' attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{SpatialSizeUnitsRemoved, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'spatialSizeUnits' is not supported in this Level+Version of SBML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{SubstanceUnitsOnSpecies, SBMLErrorCode_t}</td>
<td class="meaning">No substance units defined for the species</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{ConversionFactorOnSpecies, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the 'conversionFactor' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnSpecies, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on Species object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidParameterUnits, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the 'units' attribute of a Parameter object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{ParameterUnits, SBMLErrorCode_t}</td>
<td class="meaning">No units defined for the parameter</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{ConversionFactorMustConstant, SBMLErrorCode_t}</td>
<td class="meaning">A conversion factor must reference a Parameter object declared to be a constant</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnParameter, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on Parameter object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidInitAssignSymbol, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the 'symbol' attribute of an InitialAssignment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{MultipleInitAssignments, SBMLErrorCode_t}</td>
<td class="meaning">Multiple initial assignments for the same 'symbol' value are not allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InitAssignmentAndRuleForSameId, SBMLErrorCode_t}</td>
<td class="meaning">Cannot set a value using both an initial assignment and an assignment rule simultaneously</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneMathElementPerInitialAssign, SBMLErrorCode_t}</td>
<td class="meaning">An InitialAssignment object must contain one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnInitialAssign, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on an InitialAssignment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidAssignRuleVariable, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the 'variable' attribute of an AssignmentRule object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidRateRuleVariable, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the 'variable' attribute of a RateRule object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AssignmentToConstantEntity, SBMLErrorCode_t}</td>
<td class="meaning">An assignment rule cannot assign an entity declared to be constant</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{RateRuleForConstantEntity, SBMLErrorCode_t}</td>
<td class="meaning">A rate rule cannot assign an entity declared to be constant</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CircularRuleDependency, SBMLErrorCode_t}</td>
<td class="meaning">Circular dependencies involving rules and reactions are not permitted</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneMathElementPerRule, SBMLErrorCode_t}</td>
<td class="meaning">A rule object must contain one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnAssignRule, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on an AssignmentRule object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnRateRule, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on a RateRule object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnAlgRule, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on an AlgebraicRule object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{ConstraintMathNotBoolean, SBMLErrorCode_t}</td>
<td class="meaning">A Constraint object's <code>&lt;math&gt;</code> must evaluate to a Boolean value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{IncorrectOrderInConstraint, SBMLErrorCode_t}</td>
<td class="meaning">Subobjects inside the Constraint object are not in the prescribed order</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{ConstraintNotInXHTMLNamespace, SBMLErrorCode_t}</td>
<td class="meaning">A Constraint's Message subobject must be in the XHTML XML namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{ConstraintContainsXMLDecl, SBMLErrorCode_t}</td>
<td class="meaning">XML declarations are not permitted within Constraint's Message objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{ConstraintContainsDOCTYPE, SBMLErrorCode_t}</td>
<td class="meaning">XML DOCTYPE elements are not permitted within Constraint's Message objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidConstraintContent, SBMLErrorCode_t}</td>
<td class="meaning">Invalid content for a Constraint object's Message object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneMathElementPerConstraint, SBMLErrorCode_t}</td>
<td class="meaning">A Constraint object must contain one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneMessageElementPerConstraint, SBMLErrorCode_t}</td>
<td class="meaning">A Constraint object must contain one Message subobject</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnConstraint, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on Constraint object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoReactantsOrProducts, SBMLErrorCode_t}</td>
<td class="meaning">Cannot have a reaction with neither reactants nor products</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{IncorrectOrderInReaction, SBMLErrorCode_t}</td>
<td class="meaning">Subobjects inside the Reaction object are not in the prescribed order</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{EmptyListInReaction, SBMLErrorCode_t}</td>
<td class="meaning">Reaction components, if present, cannot be empty</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidReactantsProductsList, SBMLErrorCode_t}</td>
<td class="meaning">Invalid object found in the list of reactants or products</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidModifiersList, SBMLErrorCode_t}</td>
<td class="meaning">Invalid object found in the list of modifiers</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneSubElementPerReaction, SBMLErrorCode_t}</td>
<td class="meaning">A Reaction object can only contain one of each allowed type of object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompartmentOnReaction, SBMLErrorCode_t}</td>
<td class="meaning">Invalid value for the Reaction 'compartment' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnReaction, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute for a Reaction object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidSpeciesReference, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'species' attribute value in SpeciesReference object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BothStoichiometryAndMath, SBMLErrorCode_t}</td>
<td class="meaning">The 'stoichiometry' attribute and StoichiometryMath subobject are mutually exclusive</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnSpeciesReference, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the SpeciesReference object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnModifier, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ModifierSpeciesReference object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{UndeclaredSpeciesRef, SBMLErrorCode_t}</td>
<td class="meaning">Unknown species referenced in the kinetic law <code>&lt;math&gt;</code> formula</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{IncorrectOrderInKineticLaw, SBMLErrorCode_t}</td>
<td class="meaning">Incorrect ordering of components in the KineticLaw object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{EmptyListInKineticLaw, SBMLErrorCode_t}</td>
<td class="meaning">The list of parameters, if present, cannot be empty</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NonConstantLocalParameter, SBMLErrorCode_t}</td>
<td class="meaning">Parameters local to a KineticLaw object must have a 'constant' attribute value of 'true'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{SubsUnitsNoLongerValid, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'substanceUnits' is not supported in this Level+Version of SBML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{TimeUnitsNoLongerValid, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'timeUnits' is not supported in this Level+Version of SBML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneListOfPerKineticLaw, SBMLErrorCode_t}</td>
<td class="meaning">Only one ListOfLocalParameters object is permitted within a KineticLaw object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyLocalParamsInListOfLocalParams, SBMLErrorCode_t}</td>
<td class="meaning">Only LocalParameter, Notes and Annotation objects are allowed in ListOfLocalParameter objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfLocalParam, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfLocalParameters object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneMathPerKineticLaw, SBMLErrorCode_t}</td>
<td class="meaning">Only one <code>&lt;math&gt;</code> element is allowed in a KineticLaw object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{UndeclaredSpeciesInStoichMath, SBMLErrorCode_t}</td>
<td class="meaning">Unknown species referenced in the StoichiometryMath object's <code>&lt;math&gt;</code> formula</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnKineticLaw, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the KineticLaw object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfSpeciesRef, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfSpeciesReferences object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfMods, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfModifiers object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnLocalParameter, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the LocalParameter object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{MissingTriggerInEvent, SBMLErrorCode_t}</td>
<td class="meaning">The Event object is missing a Trigger subobject</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{TriggerMathNotBoolean, SBMLErrorCode_t}</td>
<td class="meaning">A Trigger object's <code>&lt;math&gt;</code> expression must evaluate to a Boolean value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{MissingEventAssignment, SBMLErrorCode_t}</td>
<td class="meaning">The Event object is missing an EventAssignment subobject</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{TimeUnitsEvent, SBMLErrorCode_t}</td>
<td class="meaning">Units referenced by 'timeUnits' attribute are not compatible with units of time</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{IncorrectOrderInEvent, SBMLErrorCode_t}</td>
<td class="meaning">Incorrect ordering of components in Event object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{ValuesFromTriggerTimeNeedDelay, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'useValuesFromTriggerTime'='false', but the Event object does not define a delay</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{DelayNeedsValuesFromTriggerTime, SBMLErrorCode_t}</td>
<td class="meaning">The use of a Delay object requires the Event attribute 'useValuesFromTriggerTime'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneMathPerTrigger, SBMLErrorCode_t}</td>
<td class="meaning">A Trigger object must have one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneMathPerDelay, SBMLErrorCode_t}</td>
<td class="meaning">A Delay object must have one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidEventAssignmentVariable, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'variable' attribute value in Event object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{EventAssignmentForConstantEntity, SBMLErrorCode_t}</td>
<td class="meaning">An EventAssignment object cannot assign to a component having attribute 'constant'='true'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneMathPerEventAssignment, SBMLErrorCode_t}</td>
<td class="meaning">An EventAssignment object must have one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnEventAssignment, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the EventAssignment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyOneDelayPerEvent, SBMLErrorCode_t}</td>
<td class="meaning">An Event object can only have one Delay subobject</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneListOfEventAssignmentsPerEvent, SBMLErrorCode_t}</td>
<td class="meaning">An Event object can only have one ListOfEventAssignments subobject</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyEventAssignInListOfEventAssign, SBMLErrorCode_t}</td>
<td class="meaning">Only EventAssignment, Notes and Annotation objects are allowed in ListOfEventAssignments</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnListOfEventAssign, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the ListOfEventAssignments object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnEvent, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the Event object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnTrigger, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the Trigger object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnDelay, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the Delay object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{PersistentNotBoolean, SBMLErrorCode_t}</td>
<td class="meaning">The Trigger attribute 'persistent' must evaluate to a Boolean value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InitialValueNotBoolean, SBMLErrorCode_t}</td>
<td class="meaning">The Trigger attribute 'initialValue' must evaluate to a Boolean value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OnlyOnePriorityPerEvent, SBMLErrorCode_t}</td>
<td class="meaning">An Event object can only have one Priority subobject</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{OneMathPerPriority, SBMLErrorCode_t}</td>
<td class="meaning">A Priority object must have one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AllowedAttributesOnPriority, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on the Priority object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompartmentShouldHaveSize, SBMLErrorCode_t}</td>
<td class="meaning">It's best to define a size for every compartment in a model</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{SpeciesShouldHaveValue, SBMLErrorCode_t}</td>
<td class="meaning">It's best to define an initial amount or initial concentration for every species in a model</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{ParameterShouldHaveUnits, SBMLErrorCode_t}</td>
<td class="meaning">It's best to declare units for every parameter in a model</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{LocalParameterShadowsId, SBMLErrorCode_t}</td>
<td class="meaning">Local parameters defined within a kinetic law shadow global object symbols</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{CannotConvertToL1V1, SBMLErrorCode_t}</td>
<td class="meaning">Cannot convert to SBML Level 1 Version 1</td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoEventsInL1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 does not support events</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoFunctionDefinitionsInL1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 does not support function definitions</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoConstraintsInL1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 does not support constraints</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoInitialAssignmentsInL1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 does not support initial assignments</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoSpeciesTypesInL1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 does not support species types</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoCompartmentTypeInL1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 does not support compartment types</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoNon3DCompartmentsInL1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 only supports three-dimensional compartments</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoFancyStoichiometryMathInL1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 does not support non-integer nor non-rational stoichiometry formulas</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoNonIntegerStoichiometryInL1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 does not support non-integer 'stoichiometry' attribute values</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoUnitMultipliersOrOffsetsInL1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 does not support multipliers or offsets in unit definitions</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{SpeciesCompartmentRequiredInL1, SBMLErrorCode_t}</td>
<td class="meaning">In SBML Level 1, a value for 'compartment' is mandatory in species definitions</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoSpeciesSpatialSizeUnitsInL1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 does not support species 'spatialSizeUnits' settings</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoSBOTermsInL1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 does not support the 'sboTerm' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{StrictUnitsRequiredInL1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 requires strict unit consistency</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{ConversionFactorNotInL1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 does not support the 'conversionFactor' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompartmentNotOnL1Reaction, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 1 does not support the 'compartment' attribute on Reaction objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{ExtentUnitsNotSubstance, SBMLErrorCode_t}</td>
<td class="meaning">Units of extent must be compatible with units of substance</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{GlobalUnitsNotDeclared, SBMLErrorCode_t}</td>
<td class="meaning">Global units must be refer to unit kind or unitDefinition.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{HasOnlySubstanceUnitsNotinL1, SBMLErrorCode_t}</td>
<td class="meaning">The concept of hasOnlySubstanceUnits was not available in SBML Level 1.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AvogadroNotSupported, SBMLErrorCode_t}</td>
<td class="meaning">Avogadro not supported in Levels 2 and 1.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoConstraintsInL2v1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 1 does not support Constraint objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoInitialAssignmentsInL2v1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 1 does not support InitialAssignment objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoSpeciesTypeInL2v1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 1 does not support SpeciesType objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoCompartmentTypeInL2v1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 1 does not support CompartmentType objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoSBOTermsInL2v1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 1 does not support the 'sboTerm' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoIdOnSpeciesReferenceInL2v1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 1 does not support the 'id' attribute on SpeciesReference objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoDelayedEventAssignmentInL2v1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 1 does not support the 'useValuesFromTriggerTime' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{StrictUnitsRequiredInL2v1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 1 requires strict unit consistency</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{IntegerSpatialDimensions, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 1 requires that compartments have spatial dimensions of 0-3</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{StoichiometryMathNotYetSupported, SBMLErrorCode_t}</td>
<td class="meaning">Conversion to StoichiometryMath objects not yet supported</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{PriorityLostFromL3, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 1 does not support priorities on Event objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NonPersistentNotSupported, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 1 does not support the 'persistent' attribute on Trigger objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InitialValueFalseEventNotSupported, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 1 does not support the 'initialValue' attribute on Trigger objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{SBOTermNotUniversalInL2v2, SBMLErrorCode_t}</td>
<td class="meaning">The 'sboTerm' attribute is invalid for this component in SBML Level 2 Version 2</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoUnitOffsetInL2v2, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'offset' attribute on Unit objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoKineticLawTimeUnitsInL2v2, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoKineticLawSubstanceUnitsInL2v2, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'substanceUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoDelayedEventAssignmentInL2v2, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'useValuesFromTriggerTime' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{ModelSBOBranchChangedBeyondL2v2, SBMLErrorCode_t}</td>
<td class="meaning">The allowable 'sboTerm' attribute values for Model objects differ for this SBML Level+Version</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{StrictUnitsRequiredInL2v2, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 2 requires strict unit consistency</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{StrictSBORequiredInL2v2, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 2 requires strict SBO term consistency</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{DuplicateAnnotationInvalidInL2v2, SBMLErrorCode_t}</td>
<td class="meaning">Duplicate top-level annotations are invalid in SBML Level 2 Version 2</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoUnitOffsetInL2v3, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'offset' attribute on Unit objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoKineticLawTimeUnitsInL2v3, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoKineticLawSubstanceUnitsInL2v3, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'substanceUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoSpeciesSpatialSizeUnitsInL2v3, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'spatialSizeUnit' attribute on Species objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoEventTimeUnitsInL2v3, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on Event objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoDelayedEventAssignmentInL2v3, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'useValuesFromTriggerTime' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{ModelSBOBranchChangedBeyondL2v3, SBMLErrorCode_t}</td>
<td class="meaning">The allowable 'sboTerm' attribute values for Model objects differ for this SBML Level+Version</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{StrictUnitsRequiredInL2v3, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 3 requires strict unit consistency</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{StrictSBORequiredInL2v3, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 2 Version 3 requires strict SBO term consistency</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{DuplicateAnnotationInvalidInL2v3, SBMLErrorCode_t}</td>
<td class="meaning">Duplicate top-level annotations are invalid in SBML Level 2 Version 3</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoUnitOffsetInL2v4, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'offset' attribute on Unit objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoKineticLawTimeUnitsInL2v4, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoKineticLawSubstanceUnitsInL2v4, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'substanceUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoSpeciesSpatialSizeUnitsInL2v4, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'spatialSizeUnit' attribute on Species objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoEventTimeUnitsInL2v4, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on Event objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{ModelSBOBranchChangedInL2v4, SBMLErrorCode_t}</td>
<td class="meaning">The allowable 'sboTerm' attribute values for Model objects differ for this SBML Level+Version</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{DuplicateAnnotationInvalidInL2v4, SBMLErrorCode_t}</td>
<td class="meaning">Duplicate top-level annotations are invalid in SBML Level 2 Version 4</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoSpeciesTypeInL3v1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 3 Version 1 does not support SpeciesType objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoCompartmentTypeInL3v1, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 3 Version 1 does not support CompartmentType objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoUnitOffsetInL3v1, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'offset' attribute on Unit objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoKineticLawTimeUnitsInL3v1, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoKineticLawSubstanceUnitsInL3v1, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'substanceUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoSpeciesSpatialSizeUnitsInL3v1, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'spatialSizeUnit' attribute on Species objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoEventTimeUnitsInL3v1, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on Event objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{ModelSBOBranchChangedInL3v1, SBMLErrorCode_t}</td>
<td class="meaning">The allowable 'sboTerm' attribute values for Model objects differ for this SBML Level+Version</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{DuplicateAnnotationInvalidInL3v1, SBMLErrorCode_t}</td>
<td class="meaning">Duplicate top-level annotations are invalid in SBML Level 3 Version 1</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoCompartmentOutsideInL3v1, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'outside' attribute on Compartment objects</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoStoichiometryMathInL3v1, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the StoichiometryMath object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidSBMLLevelVersion, SBMLErrorCode_t}</td>
<td class="meaning">Unknown Level+Version combination of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{AnnotationNotesNotAllowedLevel1, SBMLErrorCode_t}</td>
<td class="meaning">Annotation objects on the SBML container element are not permitted in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidRuleOrdering, SBMLErrorCode_t}</td>
<td class="meaning">Invalid ordering of rules</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{RequiredPackagePresent, SBMLErrorCode_t}</td>
<td class="meaning">The SBML document requires an SBML Level 3 package unavailable in this software</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{UnrequiredPackagePresent, SBMLErrorCode_t}</td>
<td class="meaning">The SBML document uses an SBML Level 3 package unavailable in this software</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{PackageRequiredShouldBeFalse, SBMLErrorCode_t}</td>
<td class="meaning">This package expects required to be false</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{SubsUnitsAllowedInKL, SBMLErrorCode_t}</td>
<td class="meaning">Disallowed value for attribute 'substanceUnits' on KineticLaw object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{TimeUnitsAllowedInKL, SBMLErrorCode_t}</td>
<td class="meaning">Disallowed value for attribute 'timeUnits' on KineticLaw object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{FormulaInLevel1KL, SBMLErrorCode_t}</td>
<td class="meaning">Only predefined functions are allowed in SBML Level 1 formulas</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{L3SubstanceUnitsOnModel, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'substanceUnits' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{TimeUnitsRemoved, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on Event objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadMathML, SBMLErrorCode_t}</td>
<td class="meaning">Invalid MathML expression</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FailedMathMLReadOfDouble, SBMLErrorCode_t}</td>
<td class="meaning">Missing or invalid floating-point number in MathML expression</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FailedMathMLReadOfInteger, SBMLErrorCode_t}</td>
<td class="meaning">Missing or invalid integer in MathML expression</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FailedMathMLReadOfExponential, SBMLErrorCode_t}</td>
<td class="meaning">Missing or invalid exponential expression in MathML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FailedMathMLReadOfRational, SBMLErrorCode_t}</td>
<td class="meaning">Missing or invalid rational expression in MathML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{BadMathMLNodeType, SBMLErrorCode_t}</td>
<td class="meaning">Invalid MathML element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidMathMLAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Invalid MathML attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoTimeSymbolInFunctionDef, SBMLErrorCode_t}</td>
<td class="meaning">Use of <code>&lt;csymbol&gt;</code> for 'time' not allowed within FunctionDefinition objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{NoBodyInFunctionDef, SBMLErrorCode_t}</td>
<td class="meaning">There must be a <code>&lt;lambda&gt;</code> body within the <code>&lt;math&gt;</code> element of a FunctionDefinition object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{DanglingUnitSIdRef, SBMLErrorCode_t}</td>
<td class="meaning">Units must refer to valid unit or unitDefinition</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{RDFMissingAboutTag, SBMLErrorCode_t}</td>
<td class="meaning">RDF missing the <code>&lt;about&gt;</code> tag</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{RDFEmptyAboutTag, SBMLErrorCode_t}</td>
<td class="meaning">RDF empty <code>&lt;about&gt;</code> tag</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{RDFAboutTagNotMetaid, SBMLErrorCode_t}</td>
<td class="meaning">RDF <code>&lt;about&gt;</code> tag is not metaid</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{RDFNotCompleteModelHistory, SBMLErrorCode_t}</td>
<td class="meaning">RDF does not contain valid ModelHistory</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{RDFNotModelHistory, SBMLErrorCode_t}</td>
<td class="meaning">RDF does not result in a ModelHistory</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{AnnotationNotElement, SBMLErrorCode_t}</td>
<td class="meaning">Annotation must contain element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{UndeclaredUnits, SBMLErrorCode_t}</td>
<td class="meaning">Missing unit declarations on parameters or literal numbers in expression</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{UndeclaredTimeUnitsL3, SBMLErrorCode_t}</td>
<td class="meaning">Unable to verify consistency of units: the unit of time has not been declared</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{UndeclaredExtentUnitsL3, SBMLErrorCode_t}</td>
<td class="meaning">Unable to verify consistency of units: the units of reaction extent have not been declared</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{UndeclaredObjectUnitsL3, SBMLErrorCode_t}</td>
<td class="meaning">Unable to verify consistency of units: encountered a model entity with no declared units</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{UnrecognisedSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Unrecognized 'sboTerm' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{ObseleteSBOTerm, SBMLErrorCode_t}</td>
<td class="meaning">Obsolete 'sboTerm' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{IncorrectCompartmentSpatialDimensions, SBMLErrorCode_t}</td>
<td class="meaning">In SBML Level 1, only three-dimensional compartments are allowed</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompartmentTypeNotValidAttribute, SBMLErrorCode_t}</td>
<td class="meaning">CompartmentType objects are not available in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{ConstantNotValidAttribute, SBMLErrorCode_t}</td>
<td class="meaning">This Level+Version of SBML does not support the 'constant' attribute on this component</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{MetaIdNotValidAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'metaid' is not available in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{SBOTermNotValidAttributeBeforeL2V3, SBMLErrorCode_t}</td>
<td class="meaning">The 'sboTerm' attribute is not available on this component before SBML Level 2 Version 3</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidL1CompartmentUnits, SBMLErrorCode_t}</td>
<td class="meaning">Invalid units for a compartment in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{L1V1CompartmentVolumeReqd, SBMLErrorCode_t}</td>
<td class="meaning">In SBML Level 1, a compartment's volume must be specified</td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompartmentTypeNotValidComponent, SBMLErrorCode_t}</td>
<td class="meaning">CompartmentType objects are not available in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{ConstraintNotValidComponent, SBMLErrorCode_t}</td>
<td class="meaning">Constraint objects are not available in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{EventNotValidComponent, SBMLErrorCode_t}</td>
<td class="meaning">Event objects are not available in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{SBOTermNotValidAttributeBeforeL2V2, SBMLErrorCode_t}</td>
<td class="meaning">The 'sboTerm' attribute is invalid for this component before Level 2 Version 2</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{FuncDefNotValidComponent, SBMLErrorCode_t}</td>
<td class="meaning">FunctionDefinition objects are not available in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{InitialAssignNotValidComponent, SBMLErrorCode_t}</td>
<td class="meaning">InitialAssignment objects are not available in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{VariableNotValidAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'variable' is not available on this component in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{UnitsNotValidAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'units' is not available on this component in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{ConstantSpeciesNotValidAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'constant' is not available on Species objects in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{SpatialSizeUnitsNotValidAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'spatialSizeUnits' is not available on Species objects in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{SpeciesTypeNotValidAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'speciesType' is not available on Species objects in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{HasOnlySubsUnitsNotValidAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'hasOnlySubstanceUnits' is not available on Species objects in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{IdNotValidAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'id' is not available on SpeciesReference objects in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{NameNotValidAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'name' is not available on SpeciesReference objects in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{SpeciesTypeNotValidComponent, SBMLErrorCode_t}</td>
<td class="meaning">The SpeciesType object is not supported in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{StoichiometryMathNotValidComponent, SBMLErrorCode_t}</td>
<td class="meaning">The StoichiometryMath object is not supported in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{MultiplierNotValidAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'multiplier' on Unit objects is not supported in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{OffsetNotValidAttribute, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'offset' on Unit objects is only available in SBML Level 2 Version 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{L3SpatialDimensionsUnset, SBMLErrorCode_t}</td>
<td class="meaning">No value given for 'spatialDimensions' attribute; assuming a value of 3</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{PackageConversionNotSupported, SBMLErrorCode_t}</td>
<td class="meaning">Conversion of SBML Level 3 package constructs is not yet supported</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{InvalidTargetLevelVersion, SBMLErrorCode_t}</td>
<td class="meaning">The requested SBML Level/Version combination is not known to exist</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{L3NotSupported, SBMLErrorCode_t}</td>
<td class="meaning">SBML Level 3 is not yet supported</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompUnknown, SBMLErrorCode_t}</td>
<td class="meaning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompNSUndeclared, SBMLErrorCode_t}</td>
<td class="meaning">The comp ns is not correctly declared</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompElementNotInNs, SBMLErrorCode_t}</td>
<td class="meaning">Element not in comp namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompDuplicateComponentId, SBMLErrorCode_t}</td>
<td class="meaning">Duplicate 'id' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompUniqueModelIds, SBMLErrorCode_t}</td>
<td class="meaning">Model and ExternalModelDefinitions must have unique ids</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompUniquePortIds, SBMLErrorCode_t}</td>
<td class="meaning">Ports must have unique ids</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompInvalidSIdSyntax, SBMLErrorCode_t}</td>
<td class="meaning">Invalid SId syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompInvalidSubmodelRefSyntax, SBMLErrorCode_t}</td>
<td class="meaning">Invalid submodelRef syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompInvalidDeletionSyntax, SBMLErrorCode_t}</td>
<td class="meaning">Invalid deletion syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompInvalidConversionFactorSyntax, SBMLErrorCode_t}</td>
<td class="meaning">Invalid conversionFactor syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompInvalidNameSyntax, SBMLErrorCode_t}</td>
<td class="meaning">Invalid name syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompReplacedUnitsShouldMatch, SBMLErrorCode_t}</td>
<td class="meaning">Units of replaced elements should match replacement units.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompOneListOfReplacedElements, SBMLErrorCode_t}</td>
<td class="meaning">Only one <code>&lt;listOfReplacedElements&gt;</code> allowed.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompLOReplaceElementsAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Allowed children of <code>&lt;listOfReplacedElements&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompLOReplacedElementsAllowedAttribs, SBMLErrorCode_t}</td>
<td class="meaning">Allowed <code>&lt;listOfReplacedElements&gt;</code> attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompEmptyLOReplacedElements, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;listOfReplacedElements&gt;</code> must not be empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompOneReplacedByElement, SBMLErrorCode_t}</td>
<td class="meaning">Only one <code>&lt;replacedBy&gt;</code> object allowed.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompAttributeRequiredMissing, SBMLErrorCode_t}</td>
<td class="meaning">Required comp:required attribute on <code>&lt;sbml&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompAttributeRequiredMustBeBoolean, SBMLErrorCode_t}</td>
<td class="meaning">The comp:required attribute must be Boolean</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompRequiredTrueIfElementsRemain, SBMLErrorCode_t}</td>
<td class="meaning">The comp:required attribute must be 'true' if math changes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompRequiredFalseIfAllElementsReplaced, SBMLErrorCode_t}</td>
<td class="meaning">The comp:required attribute must be 'false' if math does not change</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompOneListOfModelDefinitions, SBMLErrorCode_t}</td>
<td class="meaning">Only one <code>&lt;listOfModelDefinitions&gt;</code> allowed.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompEmptyLOModelDefs, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;listOfModelDefinitions&gt;</code> and <code>&lt;listOfExternalModelDefinitions&gt;</code> must not be empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompLOModelDefsAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Only <code>&lt;modelDefinitions&gt;</code> in <code>&lt;listOfModelDefinitions&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompLOExtModelDefsAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Only <code>&lt;externalModelDefinitions&gt;</code> in <code>&lt;listOfExternalModelDefinitions&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompLOModelDefsAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed <code>&lt;listOfModelDefinitions&gt;</code> attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompLOExtModDefsAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed <code>&lt;listOfExternalModelDefinitions&gt;</code> attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompOneListOfExtModelDefinitions, SBMLErrorCode_t}</td>
<td class="meaning">Only one <code>&lt;listOfExternalModelDefinitions&gt;</code> allowed.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompAttributeRequiredMustBeTrue, SBMLErrorCode_t}</td>
<td class="meaning">The comp:required attribute must be 'true'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompExtModDefAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed <code>&lt;externalModelDefinitions&gt;</code> core attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompExtModDefAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Allowed <code>&lt;externalModelDefinitions&gt;</code> elements</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompExtModDefAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed <code>&lt;externalModelDefinitions&gt;</code> attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompReferenceMustBeL3, SBMLErrorCode_t}</td>
<td class="meaning">External models must be L3</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompModReferenceMustIdOfModel, SBMLErrorCode_t}</td>
<td class="meaning">'modelRef' must be the 'id' of a model in the 'source' document</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompExtModMd5DoesNotMatch, SBMLErrorCode_t}</td>
<td class="meaning">MD5 checksum does not match the 'source' document</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompInvalidSourceSyntax, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:source' attribute must be of type 'anyURI'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompInvalidModelRefSyntax, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:modelRef' attribute must have the syntax of 'SId'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompInvalidMD5Syntax, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:md5' attribute must have the syntax of 'string'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompCircularExternalModelReference, SBMLErrorCode_t}</td>
<td class="meaning">Circular reference in <code>&lt;externalModelDefinition&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompOneListOfOnModel, SBMLErrorCode_t}</td>
<td class="meaning">Only one <code>&lt;listOfSubmodels&gt;</code> and one <code>&lt;listOfPorts&gt;</code> allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompNoEmptyListOfOnModel, SBMLErrorCode_t}</td>
<td class="meaning">No empty listOf elements allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompLOSubmodelsAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Allowed elements on <code>&lt;listOfSubmodels&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompLOPortsAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Allowed elements on <code>&lt;listOfPorts&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompLOSubmodelsAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed attributes on <code>&lt;listOfSubmodels&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompLOPortsAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed attributes on <code>&lt;listOfPorts&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompSubmodelAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed core attributes on <code>&lt;submodel&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompSubmodelAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Allowed elements on <code>&lt;submodel&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompOneListOfDeletionOnSubmodel, SBMLErrorCode_t}</td>
<td class="meaning">Only one <code>&lt;listOfDeletions&gt;</code> on a <code>&lt;submodel&gt;</code> allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompSubmodelNoEmptyLODeletions, SBMLErrorCode_t}</td>
<td class="meaning">No empty listOfDeletions elements allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompLODeletionsAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Allowed elements on <code>&lt;listOfDeletions&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompLODeletionAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed <code>&lt;listOfDeletions&gt;</code> attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompSubmodelAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed <code>&lt;submodel&gt;</code> attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompModReferenceSyntax, SBMLErrorCode_t}</td>
<td class="meaning">'comp:modelRef' must conform to SId syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompInvalidTimeConvFactorSyntax, SBMLErrorCode_t}</td>
<td class="meaning">'comp:timeConversionFactor' must conform to SId syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompInvalidExtentConvFactorSyntax, SBMLErrorCode_t}</td>
<td class="meaning">'comp:extentConversionFactor' must conform to SId syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompSubmodelMustReferenceModel, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:modelRef' attribute must reference a model</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompSubmodelCannotReferenceSelf, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:modelRef' attribute cannot reference own model</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompModCannotCircularlyReferenceSelf, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;model&gt;</code> may not reference <code>&lt;submodel&gt;</code> that references itself.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompTimeConversionMustBeParameter, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:timeConversionFactor' must reference a parameter</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompExtentConversionMustBeParameter, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:extentConversionFactor' must reference a parameter</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompPortRefMustReferencePort, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:portRef' attribute must be the 'id' of a <code>&lt;port&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompIdRefMustReferenceObject, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:idRef' attribute must be the 'id' of a model element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompUnitRefMustReferenceUnitDef, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:unitRef' attribute must be the 'id' of a UnitDefinition</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompMetaIdRefMustReferenceObject, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:metaIdRef' attribute must be the 'metaid' of an object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompParentOfSBRefChildMustBeSubmodel, SBMLErrorCode_t}</td>
<td class="meaning">If <code>&lt;sBaseRef&gt;</code> has a child <code>&lt;sBaseRef&gt;</code> its parent must be a <code>&lt;submodel&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompInvalidPortRefSyntax, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:portRef' attribute must have the syntax of an SBML SId</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompInvalidIdRefSyntax, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:idRef' attribute must have the syntax of an SBML SId</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompInvalidUnitRefSyntax, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:unitRef' attribute must have the syntax of an SBML SId</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompInvalidMetaIdRefSyntax, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:metaIdRef' attribute must have the syntax of an XML ID</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompOneSBaseRefOnly, SBMLErrorCode_t}</td>
<td class="meaning">Only one <code>&lt;sbaseRef&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompDeprecatedSBaseRefSpelling, SBMLErrorCode_t}</td>
<td class="meaning">The spelling 'sbaseRef' is deprecated</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompSBaseRefMustReferenceObject, SBMLErrorCode_t}</td>
<td class="meaning">An SBaseRef must reference an object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompSBaseRefMustReferenceOnlyOneObject, SBMLErrorCode_t}</td>
<td class="meaning">An SBaseRef must reference only one other object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompNoMultipleReferences, SBMLErrorCode_t}</td>
<td class="meaning">Objects may not be referenced by mutiple SBaseRef constructs.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompPortMustReferenceObject, SBMLErrorCode_t}</td>
<td class="meaning">Port must reference an object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompPortMustReferenceOnlyOneObject, SBMLErrorCode_t}</td>
<td class="meaning">Port must reference only one other object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompPortAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed attributes on a Port</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompPortReferencesUnique, SBMLErrorCode_t}</td>
<td class="meaning">Port definitions must be unique.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompDeletionMustReferenceObject, SBMLErrorCode_t}</td>
<td class="meaning">Deletion must reference an object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompDeletionMustReferOnlyOneObject, SBMLErrorCode_t}</td>
<td class="meaning">Deletion must reference only one other object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompDeletionAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed attributes on a Deletion</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompReplacedElementMustRefObject, SBMLErrorCode_t}</td>
<td class="meaning">ReplacedElement must reference an object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompReplacedElementMustRefOnlyOne, SBMLErrorCode_t}</td>
<td class="meaning">ReplacedElement must reference only one other object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompReplacedElementAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed attributes on <code>&lt;replacedElement&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompReplacedElementSubModelRef, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:submodelRef' attribute must point to a <code>&lt;submodel&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompReplacedElementDeletionRef, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:deletion' attribute must point to a <code>&lt;deletion&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompReplacedElementConvFactorRef, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:conversionFactor attribute must point to a <code>&lt;parameter&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompReplacedElementSameReference, SBMLErrorCode_t}</td>
<td class="meaning">No <code>&lt;replacedElement&gt;</code> refer to same object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompReplacedElementNoDelAndConvFact, SBMLErrorCode_t}</td>
<td class="meaning">No <code>&lt;replacedElement&gt;</code> with deletion and conversionfactor</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompReplacedByMustRefObject, SBMLErrorCode_t}</td>
<td class="meaning">ReplacedBy must reference an object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompReplacedByMustRefOnlyOne, SBMLErrorCode_t}</td>
<td class="meaning">ReplacedBy must reference only one other object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompReplacedByAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed attributes on <code>&lt;replacedBy&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompReplacedBySubModelRef, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:submodelRef' attribute must point to a <code>&lt;submodel&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompMustReplaceSameClass, SBMLErrorCode_t}</td>
<td class="meaning">Replaced classes must match.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompMustReplaceIDs, SBMLErrorCode_t}</td>
<td class="meaning">Replaced IDs must be replaced with IDs.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompMustReplaceMetaIDs, SBMLErrorCode_t}</td>
<td class="meaning">Replaced metaids must be replaced with metaids.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompMustReplacePackageIDs, SBMLErrorCode_t}</td>
<td class="meaning">Replaced package IDs must be replaced with package IDs.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompUnresolvedReference, SBMLErrorCode_t}</td>
<td class="meaning">Unresolved reference.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompNoModelInReference, SBMLErrorCode_t}</td>
<td class="meaning">No model in referenced document.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompExtModDefBad, SBMLErrorCode_t}</td>
<td class="meaning">Referenced <code>&lt;externalModelDefinition&gt;</code> unresolvable.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompModelFlatteningFailed, SBMLErrorCode_t}</td>
<td class="meaning">Model failed to flatten.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompFlatModelNotValid, SBMLErrorCode_t}</td>
<td class="meaning">Flat model not valid.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompLineNumbersUnreliable, SBMLErrorCode_t}</td>
<td class="meaning">Line numbers unreliable.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompFlatteningNotRecognisedReqd, SBMLErrorCode_t}</td>
<td class="meaning">Flattening not implemented for required package.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompFlatteningNotRecognisedNotReqd, SBMLErrorCode_t}</td>
<td class="meaning">Flattening not implemented for unrequired package.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompFlatteningNotImplementedNotReqd, SBMLErrorCode_t}</td>
<td class="meaning">Flattening not implemented for unrequired package.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompFlatteningNotImplementedReqd, SBMLErrorCode_t}</td>
<td class="meaning">Flattening not implemented for required package.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompFlatteningWarning, SBMLErrorCode_t}</td>
<td class="meaning">Flattening reference may come from package.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompDeprecatedDeleteFunction, SBMLErrorCode_t}</td>
<td class="meaning">The performDeletions functions is deprecated.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompDeprecatedReplaceFunction, SBMLErrorCode_t}</td>
<td class="meaning">The performReplacementsAndConversions fuctions is deprecated.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompDeletedReplacement, SBMLErrorCode_t}</td>
<td class="meaning">Element deleted before a subelement could be replaced.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompIdRefMayReferenceUnknownPackage, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:idRef' attribute must be the 'id' of a model element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{CompMetaIdRefMayReferenceUnknownPkg, SBMLErrorCode_t}</td>
<td class="meaning">The 'comp:metaIdRef' attribute must be the 'metaid' of a model element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcUnknown, SBMLErrorCode_t}</td>
<td class="meaning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcNSUndeclared, SBMLErrorCode_t}</td>
<td class="meaning">The fbc ns is not correctly declared</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcElementNotInNs, SBMLErrorCode_t}</td>
<td class="meaning">Element not in fbc namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcDuplicateComponentId, SBMLErrorCode_t}</td>
<td class="meaning">Duplicate 'id' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcSBMLSIdSyntax, SBMLErrorCode_t}</td>
<td class="meaning">Invalid 'id' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcAttributeRequiredMissing, SBMLErrorCode_t}</td>
<td class="meaning">Required fbc:required attribute on <code>&lt;sbml&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcAttributeRequiredMustBeBoolean, SBMLErrorCode_t}</td>
<td class="meaning">The fbc:required attribute must be Boolean</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcRequiredFalse, SBMLErrorCode_t}</td>
<td class="meaning">The fbc:required attribute must be 'false'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcOnlyOneEachListOf, SBMLErrorCode_t}</td>
<td class="meaning">One of each list of allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcNoEmptyListOfs, SBMLErrorCode_t}</td>
<td class="meaning">ListOf elements cannot be empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcLOFluxBoundsAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Allowed elements on ListOfFluxBounds</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcLOObjectivesAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Allowed elements on ListOfObjectives</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcLOFluxBoundsAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed attributes on ListOfFluxBounds</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcLOObjectivesAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed attributes on ListOfObjectives</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcActiveObjectiveSyntax, SBMLErrorCode_t}</td>
<td class="meaning">Type of activeObjective attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcActiveObjectiveRefersObjective, SBMLErrorCode_t}</td>
<td class="meaning">ActiveObjective must reference Objective</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcSpeciesAllowedL3Attributes, SBMLErrorCode_t}</td>
<td class="meaning">Species allowed attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcSpeciesChargeMustBeInteger, SBMLErrorCode_t}</td>
<td class="meaning">Charge must be integer</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcSpeciesFormulaMustBeString, SBMLErrorCode_t}</td>
<td class="meaning">Chemical formula must be string</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxBoundAllowedL3Attributes, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;fluxBound&gt;</code> may only have 'metaId' and 'sboTerm' from L3 namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxBoundAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;fluxBound&gt;</code> may only have <code>&lt;notes&gt;</code> and <code>&lt;annotations&gt;</code> from L3 Core</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxBoundRequiredAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on <code>&lt;fluxBound&gt;</code> object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxBoundRectionMustBeSIdRef, SBMLErrorCode_t}</td>
<td class="meaning">Datatype for 'fbc:reaction' must be SIdRef</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxBoundNameMustBeString, SBMLErrorCode_t}</td>
<td class="meaning">The attribute 'fbc:name' must be of the data type string</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxBoundOperationMustBeEnum, SBMLErrorCode_t}</td>
<td class="meaning">The attribute 'fbc:operation' must be of data type FbcOperation</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxBoundValueMustBeDouble, SBMLErrorCode_t}</td>
<td class="meaning">The attribute 'fbc:value' must be of the data type double</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxBoundReactionMustExist, SBMLErrorCode_t}</td>
<td class="meaning">'fbc:reaction' must refer to valid reaction</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxBoundsForReactionConflict, SBMLErrorCode_t}</td>
<td class="meaning">Conflicting set of FluxBounds for a reaction</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcObjectiveAllowedL3Attributes, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;objective&gt;</code> may only have 'metaId' and 'sboTerm' from L3 namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcObjectiveAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;objective&gt;</code> may only have <code>&lt;notes&gt;</code> and <code>&lt;annotations&gt;</code> from L3 Core</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcObjectiveRequiredAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on <code>&lt;objective&gt;</code> object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcObjectiveNameMustBeString, SBMLErrorCode_t}</td>
<td class="meaning">The attribute 'fbc:name' must be of the data type string</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcObjectiveTypeMustBeEnum, SBMLErrorCode_t}</td>
<td class="meaning">The attribute 'fbc:type' must be of data type FbcType.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcObjectiveOneListOfObjectives, SBMLErrorCode_t}</td>
<td class="meaning">An <code>&lt;objective&gt;</code> must have one <code>&lt;listOfFluxObjectives&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcObjectiveLOFluxObjMustNotBeEmpty, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;listOfFluxObjectives&gt;</code> subobject must not be empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcObjectiveLOFluxObjOnlyFluxObj, SBMLErrorCode_t}</td>
<td class="meaning">Invalid element found in <code>&lt;listOfFluxObjectives&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcObjectiveLOFluxObjAllowedAttribs, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;listOfFluxObjectives&gt;</code> may only have 'metaId' and 'sboTerm' from L3 core</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxObjectAllowedL3Attributes, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;fluxObjective&gt;</code> may only have 'metaId' and 'sboTerm' from L3 namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxObjectAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;fluxObjective&gt;</code> may only have <code>&lt;notes&gt;</code> and <code>&lt;annotations&gt;</code> from L3 Core</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxObjectRequiredAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Invalid attribute found on <code>&lt;fluxObjective&gt;</code> object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxObjectNameMustBeString, SBMLErrorCode_t}</td>
<td class="meaning">The attribute 'fbc:name' must be of the data type string</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxObjectReactionMustBeSIdRef, SBMLErrorCode_t}</td>
<td class="meaning">Datatype for 'fbc:reaction' must be SIdRef</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxObjectReactionMustExist, SBMLErrorCode_t}</td>
<td class="meaning">'fbc:reaction' must refer to valid reaction</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{FbcFluxObjectCoefficientMustBeDouble, SBMLErrorCode_t}</td>
<td class="meaning">The attribute 'fbc:coefficient' must be of the data type double</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualUnknown, SBMLErrorCode_t}</td>
<td class="meaning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualNSUndeclared, SBMLErrorCode_t}</td>
<td class="meaning">The qual ns is not correctly declared</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualElementNotInNs, SBMLErrorCode_t}</td>
<td class="meaning">Element not in qual namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualFunctionTermBool, SBMLErrorCode_t}</td>
<td class="meaning">FunctionTerm should return boolean</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualMathCSymbolDisallowed, SBMLErrorCode_t}</td>
<td class="meaning">CSymbol time or delay not allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualDuplicateComponentId, SBMLErrorCode_t}</td>
<td class="meaning">Duplicate 'id' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualAttributeRequiredMissing, SBMLErrorCode_t}</td>
<td class="meaning">Required qual:required attribute on <code>&lt;sbml&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualAttributeRequiredMustBeBoolean, SBMLErrorCode_t}</td>
<td class="meaning">The qual:required attribute must be Boolean</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualRequiredTrueIfTransitions, SBMLErrorCode_t}</td>
<td class="meaning">The qual:required attribute must be 'true' if math changes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualOneListOfTransOrQS, SBMLErrorCode_t}</td>
<td class="meaning">Only one <code>&lt;listOfTransitions&gt;</code> or <code>&lt;listOfQualitativeSpecies&gt;</code> allowed.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualEmptyLONotAllowed, SBMLErrorCode_t}</td>
<td class="meaning">Empty <code>&lt;listOfTransitions&gt;</code> or <code>&lt;listOfQualitativeSpecies&gt;</code> not allowed.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualLOTransitiondAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements allowed on <code>&lt;listOfTransitions&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualLOQualSpeciesAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements allowed on <code>&lt;listOfTransitions&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualLOQualSpeciesAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfQualitativeSpecies&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualLOTransitionsAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfTransitions&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualQualSpeciesAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;qualitativeSpecies&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualQualSpeciesAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements allowed on <code>&lt;qualitativeSpecies&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualQualSpeciesAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;qualitativeSpecies&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualConstantMustBeBool, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'constant' on <code>&lt;qualitativeSpecies&gt;</code> must be boolean.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualNameMustBeString, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'name' on <code>&lt;qualitativeSpecies&gt;</code> must be string.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualInitialLevelMustBeInt, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'initialLevel' on <code>&lt;qualitativeSpecies&gt;</code> must be integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualMaxLevelMustBeInt, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'maxLevel' on <code>&lt;qualitativeSpecies&gt;</code> must be integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualCompartmentMustReferExisting, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'compartment' on <code>&lt;qualitativeSpecies&gt;</code> must reference compartment.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualInitialLevelCannotExceedMax, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'initialLevel' on <code>&lt;qualitativeSpecies&gt;</code> cannot exceed maxLevel.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualConstantQSCannotBeOutput, SBMLErrorCode_t}</td>
<td class="meaning">Constant <code>&lt;qualitativeSpecies&gt;</code> cannot be an Output.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualQSAssignedOnlyOnce, SBMLErrorCode_t}</td>
<td class="meaning">A <code>&lt;qualitativeSpecies&gt;</code> can only be assigned once.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualInitalLevelNotNegative, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'initialLevel' on <code>&lt;qualitativeSpecies&gt;</code> cannot be negative.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualMaxLevelNotNegative, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'maxLevel' on <code>&lt;qualitativeSpecies&gt;</code> cannot be negative.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualTransitionAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;transition&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualTransitionAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements allowed on <code>&lt;transition&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualTransitionAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;transition&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualTransitionNameMustBeString, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'name' on <code>&lt;transition&gt;</code> must be string.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualTransitionLOElements, SBMLErrorCode_t}</td>
<td class="meaning">ListOf elements on <code>&lt;transition&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualTransitionEmptyLOElements, SBMLErrorCode_t}</td>
<td class="meaning">ListOf elements on <code>&lt;transition&gt;</code> not empty.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualTransitionLOInputElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements on <code>&lt;listOfInputs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualTransitionLOOutputElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements on <code>&lt;listOfOutputs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualTransitionLOFuncTermElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements on <code>&lt;listOfFunctionTerms&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualTransitionLOInputAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfInputs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualTransitionLOOutputAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfOutputs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualTransitionLOFuncTermAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfFunctionTerms&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualTransitionLOFuncTermExceedMax, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;listOfFunctionTerms&gt;</code> cannot make qualitativeSpecies exceed maxLevel.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualTransitionLOFuncTermNegative, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;listOfFunctionTerms&gt;</code> cannot make qualitativeSpecies negative.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualInputAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;input&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualInputAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements allowed on <code>&lt;input&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualInputAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;input&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualInputNameMustBeString, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'name' on <code>&lt;input&gt;</code> must be string.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualInputSignMustBeSignEnum, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'sign' on <code>&lt;input&gt;</code> must be enum.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualInputTransEffectMustBeInputEffect, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'transitionEffect' on <code>&lt;input&gt;</code> must be enum.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualInputThreshMustBeInteger, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'thresholdLevel' on <code>&lt;input&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualInputQSMustBeExistingQS, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'qualitativeSpecies' on <code>&lt;input&gt;</code> must refer to existing</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualInputConstantCannotBeConsumed, SBMLErrorCode_t}</td>
<td class="meaning">Constant <code>&lt;input&gt;</code> cannot be consumed.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualInputThreshMustBeNonNegative, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'thresholdLevel' on <code>&lt;input&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualOutputAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;output&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualOutputAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements allowed on <code>&lt;output&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualOutputAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;output&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualOutputNameMustBeString, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'name' on <code>&lt;output&gt;</code> must be string.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualOutputTransEffectMustBeOutput, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'transitionEffect' on <code>&lt;output&gt;</code> must be enum.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualOutputLevelMustBeInteger, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'outputLevel' on <code>&lt;output&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualOutputQSMustBeExistingQS, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'qualitativeSpecies' on <code>&lt;output&gt;</code> must refer to existing</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualOutputConstantMustBeFalse, SBMLErrorCode_t}</td>
<td class="meaning">Constant 'qualitativeSpecies' cannot be <code>&lt;output&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualOutputProductionMustHaveLevel, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;output&gt;</code> being produced must have level</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualOutputLevelMustBeNonNegative, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'outputLevel' on <code>&lt;output&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualDefaultTermAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;defaultTerm&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualDefaultTermAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements allowed on <code>&lt;defaultTerm&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualDefaultTermAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;defaultTerm&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualDefaultTermResultMustBeInteger, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'resultLevel' on <code>&lt;defaultTerm&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualDefaultTermResultMustBeNonNeg, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'resultLevel' on <code>&lt;defaultTerm&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualFuncTermAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;functionTerm&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualFuncTermAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements allowed on <code>&lt;functionTerm&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualFuncTermAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;functionTerm&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualFuncTermOnlyOneMath, SBMLErrorCode_t}</td>
<td class="meaning">Only one <code>&lt;math&gt;</code> on <code>&lt;functionTerm&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualFuncTermResultMustBeInteger, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'resultLevel' on <code>&lt;functionTerm&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{QualFuncTermResultMustBeNonNeg, SBMLErrorCode_t}</td>
<td class="meaning">Attribute 'resultLevel' on <code>&lt;functionTerm&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutUnknownError, SBMLErrorCode_t}</td>
<td class="meaning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutNSUndeclared, SBMLErrorCode_t}</td>
<td class="meaning">The layout ns is not correctly declared</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutElementNotInNs, SBMLErrorCode_t}</td>
<td class="meaning">Element not in layout namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutDuplicateComponentId, SBMLErrorCode_t}</td>
<td class="meaning">Duplicate 'id' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSIdSyntax, SBMLErrorCode_t}</td>
<td class="meaning">'id' attribute incorrect syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutXsiTypeAllowedLocations, SBMLErrorCode_t}</td>
<td class="meaning">'xsi:type' allowed locations</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutXsiTypeSyntax, SBMLErrorCode_t}</td>
<td class="meaning">'xsi:type' attribute incorrect syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutAttributeRequiredMissing, SBMLErrorCode_t}</td>
<td class="meaning">Required layout:required attribute on <code>&lt;sbml&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutAttributeRequiredMustBeBoolean, SBMLErrorCode_t}</td>
<td class="meaning">The layout:required attribute must be Boolean</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutRequiredFalse, SBMLErrorCode_t}</td>
<td class="meaning">The layout:required attribute must be 'false'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutOnlyOneLOLayouts, SBMLErrorCode_t}</td>
<td class="meaning">Only one listOfLayouts on <code>&lt;model&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOLayoutsNotEmpty, SBMLErrorCode_t}</td>
<td class="meaning">ListOf elements cannot be empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOLayoutsAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Allowed elements on ListOfLayouts</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOLayoutsAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed attributes on ListOfLayouts</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLayoutAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Allowed elements on Layout</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLayoutAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed core attributes on Layout</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutOnlyOneEachListOf, SBMLErrorCode_t}</td>
<td class="meaning">Only one each listOf on <code>&lt;layout&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutNoEmptyListOfs, SBMLErrorCode_t}</td>
<td class="meaning">ListOf elements cannot be empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLayoutAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning"><code>&lt;layout&gt;</code> must have 'id' and may have 'name'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLayoutNameMustBeString, SBMLErrorCode_t}</td>
<td class="meaning">'name' must be string</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOCompGlyphAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfCompartmentGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOCompGlyphAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements allowed on <code>&lt;listOfCompartmentGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOSpeciesGlyphAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfSpeciesGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOSpeciesGlyphAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements allowed on <code>&lt;listOfSpeciesGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLORnGlyphAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfReactionGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLORnGlyphAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements allowed on <code>&lt;listOfReactionGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOAddGOAllowedAttribut, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfAdditionalGraphicalObjectGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOAddGOAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements allowed on <code>&lt;listOfAdditionalGraphicalObjectGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLayoutMustHaveDimensions, SBMLErrorCode_t}</td>
<td class="meaning">Layout must have <code>&lt;dimensions&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOTextGlyphAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfTextGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOTextGlyphAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Elements allowed on <code>&lt;listOfTextGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGOAllowedCoreElements, SBMLErrorCode_t}</td>
<td class="meaning">Core elements allowed on <code>&lt;graphicalObject&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGOAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;graphicalObject&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGOAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Layout elements allowed on <code>&lt;graphicalObject&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGOAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Layout attributes allowed on <code>&lt;graphicalObject&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGOMetaIdRefMustBeIDREF, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGOMetaIdRefMustReferenceObject, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGOMustContainBoundingBox, SBMLErrorCode_t}</td>
<td class="meaning">A <code>&lt;graphicalObject&gt;</code> must contain a <code>&lt;boundingBox&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCGAllowedCoreElements, SBMLErrorCode_t}</td>
<td class="meaning">Core elements allowed on <code>&lt;compartmentGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCGAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;compartmentGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCGAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Layout elements allowed on <code>&lt;compartmentGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCGAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Layout attributes allowed on <code>&lt;compartmentGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCGMetaIdRefMustBeIDREF, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCGMetaIdRefMustReferenceObject, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCGCompartmentSyntax, SBMLErrorCode_t}</td>
<td class="meaning">CompartmentGlyph 'compartment' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCGCompartmentMustRefComp, SBMLErrorCode_t}</td>
<td class="meaning">CompartmentGlyph compartment must reference existing compartment.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCGNoDuplicateReferences, SBMLErrorCode_t}</td>
<td class="meaning">CompartmentGlyph cannot reference two objects.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCGOrderMustBeDouble, SBMLErrorCode_t}</td>
<td class="meaning">CompartmentGlyph order must be double.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSGAllowedCoreElements, SBMLErrorCode_t}</td>
<td class="meaning">Core elements allowed on <code>&lt;speciesGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSGAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;speciesGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSGAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Layout elements allowed on <code>&lt;speciesGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSGAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Layout attributes allowed on <code>&lt;speciesGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSGMetaIdRefMustBeIDREF, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSGMetaIdRefMustReferenceObject, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSGSpeciesSyntax, SBMLErrorCode_t}</td>
<td class="meaning">SpeciesGlyph 'species' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSGSpeciesMustRefSpecies, SBMLErrorCode_t}</td>
<td class="meaning">SpeciesGlyph species must reference existing species.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSGNoDuplicateReferences, SBMLErrorCode_t}</td>
<td class="meaning">SpeciesGlyph cannot reference two objects.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutRGAllowedCoreElements, SBMLErrorCode_t}</td>
<td class="meaning">Core elements allowed on <code>&lt;reactionGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutRGAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;reactionGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutRGAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Layout elements allowed on <code>&lt;reactionGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutRGAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Layout attributes allowed on <code>&lt;reactionGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutRGMetaIdRefMustBeIDREF, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutRGMetaIdRefMustReferenceObject, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutRGReactionSyntax, SBMLErrorCode_t}</td>
<td class="meaning">ReactionGlyph 'reaction' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutRGReactionMustRefReaction, SBMLErrorCode_t}</td>
<td class="meaning">ReactionGlyph reaction must reference existing reaction.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutRGNoDuplicateReferences, SBMLErrorCode_t}</td>
<td class="meaning">ReactionGlyph cannot reference two objects.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOSpeciesRefGlyphAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Allowed elements on ListOfSpeciesReferenceGlyphs</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOSpeciesRefGlyphAllowedAttribs, SBMLErrorCode_t}</td>
<td class="meaning">Allowed attributes on ListOfSpeciesReferenceGlyphs</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOSpeciesRefGlyphNotEmpty, SBMLErrorCode_t}</td>
<td class="meaning">ListOfSpeciesReferenceGlyphs not empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGGAllowedCoreElements, SBMLErrorCode_t}</td>
<td class="meaning">Core elements allowed on <code>&lt;generalGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGGAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;generalGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGGAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Layout elements allowed on <code>&lt;generalGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGGAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Layout attributes allowed on <code>&lt;generalGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGGMetaIdRefMustBeIDREF, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGGMetaIdRefMustReferenceObject, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGGReferenceSyntax, SBMLErrorCode_t}</td>
<td class="meaning">GeneralGlyph 'reference' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGGReferenceMustRefObject, SBMLErrorCode_t}</td>
<td class="meaning">GeneralGlyph 'reference' must reference existing element.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutGGNoDuplicateReferences, SBMLErrorCode_t}</td>
<td class="meaning">GeneralGlyph cannot reference two objects.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOReferenceGlyphAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Allowed elements on ListOfReferenceGlyphs</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOReferenceGlyphAllowedAttribs, SBMLErrorCode_t}</td>
<td class="meaning">Allowed attributes on ListOfReferenceGlyphs</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOSubGlyphAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOSubGlyphAllowedAttribs, SBMLErrorCode_t}</td>
<td class="meaning">Allowed attributes on ListOfSubGlyphs</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutTGAllowedCoreElements, SBMLErrorCode_t}</td>
<td class="meaning">Core elements allowed on <code>&lt;textGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutTGAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;textGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutTGAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Layout elements allowed on <code>&lt;textGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutTGAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Layout attributes allowed on <code>&lt;textGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutTGMetaIdRefMustBeIDREF, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutTGMetaIdRefMustReferenceObject, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutTGOriginOfTextSyntax, SBMLErrorCode_t}</td>
<td class="meaning">TextGlyph 'originOfText' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutTGOriginOfTextMustRefObject, SBMLErrorCode_t}</td>
<td class="meaning">TextGlyph 'originOfText' must reference existing element.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutTGNoDuplicateReferences, SBMLErrorCode_t}</td>
<td class="meaning">TextGlyph cannot reference two objects.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutTGGraphicalObjectSyntax, SBMLErrorCode_t}</td>
<td class="meaning">TextGlyph 'graphicalObject' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutTGGraphicalObjectMustRefObject, SBMLErrorCode_t}</td>
<td class="meaning">TextGlyph 'graphicalObject' must reference existing element.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutTGTextMustBeString, SBMLErrorCode_t}</td>
<td class="meaning">TextGlyph 'text' must be string.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSRGAllowedCoreElements, SBMLErrorCode_t}</td>
<td class="meaning">Core elements allowed on <code>&lt;speciesReferenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSRGAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;speciesReferenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSRGAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Layout elements allowed on <code>&lt;speciesReferenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSRGAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Layout attributes allowed on <code>&lt;speciesReferenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSRGMetaIdRefMustBeIDREF, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSRGMetaIdRefMustReferenceObject, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSRGSpeciesReferenceSyntax, SBMLErrorCode_t}</td>
<td class="meaning">SpeciesReferenceGlyph 'speciesReference' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSRGSpeciesRefMustRefObject, SBMLErrorCode_t}</td>
<td class="meaning">SpeciesReferenceGlyph 'speciesReference' must reference existing element.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSRGNoDuplicateReferences, SBMLErrorCode_t}</td>
<td class="meaning">SpeciesReferenceGlyph cannot reference two objects.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSRGSpeciesGlyphSyntax, SBMLErrorCode_t}</td>
<td class="meaning">SpeciesReferenceGlyph 'speciesGlyph' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSRGSpeciesGlyphMustRefObject, SBMLErrorCode_t}</td>
<td class="meaning">SpeciesReferenceGlyph 'speciesGlyph' must reference existing element.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutSRGRoleSyntax, SBMLErrorCode_t}</td>
<td class="meaning">SpeciesReferenceGlyph 'role' must be string from enumeration.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutREFGAllowedCoreElements, SBMLErrorCode_t}</td>
<td class="meaning">Core elements allowed on <code>&lt;referenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutREFGAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;referenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutREFGAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Layout elements allowed on <code>&lt;referenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutREFGAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Layout attributes allowed on <code>&lt;referenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutREFGMetaIdRefMustBeIDREF, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutREFGMetaIdRefMustReferenceObject, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutREFGReferenceSyntax, SBMLErrorCode_t}</td>
<td class="meaning">ReferenceGlyph 'reference' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutREFGReferenceMustRefObject, SBMLErrorCode_t}</td>
<td class="meaning">ReferenceGlyph 'reference' must reference existing element.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutREFGNoDuplicateReferences, SBMLErrorCode_t}</td>
<td class="meaning">ReferenceGlyph cannot reference two objects.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutREFGGlyphSyntax, SBMLErrorCode_t}</td>
<td class="meaning">ReferenceGlyph 'glyph' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutREFGGlyphMustRefObject, SBMLErrorCode_t}</td>
<td class="meaning">ReferenceGlyph 'glyph' must reference existing element.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutREFGRoleSyntax, SBMLErrorCode_t}</td>
<td class="meaning">ReferenceGlyph 'role' must be string.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutPointAllowedCoreElements, SBMLErrorCode_t}</td>
<td class="meaning">Core elements allowed on <code>&lt;point&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutPointAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;point&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutPointAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Layout attributes allowed on <code>&lt;point&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutPointAttributesMustBeDouble, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'x', 'y' and 'z' must be double.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutBBoxAllowedCoreElements, SBMLErrorCode_t}</td>
<td class="meaning">Core elements allowed on <code>&lt;boundingBox&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutBBoxAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;boundingBox&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutBBoxAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Layout elements allowed on <code>&lt;boundingBox&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutBBoxAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Layout attributes allowed on <code>&lt;boundingBox&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutBBoxConsistent3DDefinition, SBMLErrorCode_t}</td>
<td class="meaning">Layout consistent dimensions on a <code>&lt;boundingBox&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCurveAllowedCoreElements, SBMLErrorCode_t}</td>
<td class="meaning">Core elements allowed on <code>&lt;curve&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCurveAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;curve&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCurveAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Layout elements allowed on <code>&lt;curve&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCurveAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Layout attributes allowed on <code>&lt;curve&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOCurveSegsAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Allowed attributes on ListOfCurveSegments</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOCurveSegsAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Allowed elements on ListOfCurveSegments</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLOCurveSegsNotEmpty, SBMLErrorCode_t}</td>
<td class="meaning">No empty ListOfCurveSegments</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLSegAllowedCoreElements, SBMLErrorCode_t}</td>
<td class="meaning">Core elements allowed on <code>&lt;lineSegment&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLSegAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;lineSegment&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLSegAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Layout elements allowed on <code>&lt;lineSegment&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutLSegAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Layout attributes allowed on <code>&lt;lineSegment&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCBezAllowedCoreElements, SBMLErrorCode_t}</td>
<td class="meaning">Core elements allowed on <code>&lt;cubicBezier&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCBezAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;cubicBezier&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCBezAllowedElements, SBMLErrorCode_t}</td>
<td class="meaning">Layout elements allowed on <code>&lt;cubicBezier&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutCBezAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Layout attributes allowed on <code>&lt;cubicBezier&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutDimsAllowedCoreElements, SBMLErrorCode_t}</td>
<td class="meaning">Core elements allowed on <code>&lt;dimensions&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutDimsAllowedCoreAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Core attributes allowed on <code>&lt;dimensions&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutDimsAllowedAttributes, SBMLErrorCode_t}</td>
<td class="meaning">Layout attributes allowed on <code>&lt;dimensions&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@sbmlconstant{LayoutDimsAttributesMustBeDouble, SBMLErrorCode_t}</td>
<td class="meaning">Layout 'width', 'height' and 'depth' must be double.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
</table>
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_note_static_methods
 *
 * @if python @note Because this is a static method on a class, the Python
 * language interface for libSBML will contain two variants.  One will be the
 * expected, normal static method on the class (i.e., a regular
 * <em>methodName</em>), and the other will be a standalone top-level
 * function with the name <em>ClassName_methodName()</em>. This is merely an
 * artifact of how the language interfaces are created in libSBML.  The
 * methods are functionally identical. @endif@~
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_note_setting_lv
 *
 * @note Attempting to add an object to an SBMLDocument having a different
 * combination of SBML Level, Version and XML namespaces than the object
 * itself will result in an error at the time a caller attempts to make the
 * addition.  A parent object must have compatible Level, Version and XML
 * namespaces.  (Strictly speaking, a parent may also have more XML
 * namespaces than a child, but the reverse is not permitted.)  The
 * restriction is necessary to ensure that an SBML model has a consistent
 * overall structure.  This requires callers to manage their objects
 * carefully, but the benefit is increased flexibility in how models can be
 * created by permitting callers to create objects bottom-up if desired.  In
 * situations where objects are not yet attached to parents (e.g.,
 * SBMLDocument), knowledge of the intented SBML Level and Version help
 * libSBML determine such things as whether it is valid to assign a
 * particular value to an attribute.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_what_is_user_data
 *
 * @par
 * The user data associated with an SBML object can be used by an application
 * developer to attach custom information to that object in the model.  In case
 * of a deep copy, this attribute will passed as it is.  The attribute will never
 * be interpreted by libSBML.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_renamesidref_common
 *
 * Replaces all uses of a given @c SIdRef type attribute value with another
 * value.
 *
 * @copydetails doc_what_is_sidref
 *
 * This method works by looking at all attributes and (if appropriate)
 * mathematical formulas in MathML content, comparing the referenced
 * identifiers to the value of @p oldid.  If any matches are found, the
 * matching values are replaced with @p newid.  The method does @em not
 * descend into child elements.
 *
 * @param oldid the old identifier
 * @param newid the new identifier
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_renameunitsidref_common
 *
 * Replaces all uses of a given @c UnitSIdRef type attribute value with
 * another value.
 *
 * @copydetails doc_what_is_unitsidref
 *
 * This method works by looking at all unit identifier attribute values
 * (including, if appropriate, inside mathematical formulas), comparing the
 * referenced unit identifiers to the value of @p oldid.  If any matches
 * are found, the matching values are replaced with @p newid.  The method
 * does @em not descend into child elements.
 *
 * @param oldid the old identifier
 * @param newid the new identifier
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_renamemetasidref_common
 *
 * Replaces all uses of a given meta identifier attribute value with
 * another value.
 *
 * @copydetails doc_what_is_metaidref
 *
 * This method works by looking at all meta-identifier attribute values,
 * comparing the identifiers to the value of @p oldid.  If any matches are
 * found, the matching identifiers are replaced with @p newid.  The method
 * does @em not descend into child elements.
 *
 * @param oldid the old identifier
 * @param newid the new identifier
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_section_using_sbml_converters
 *
 * @section using-converters General information about the use of SBML converters
 *
 * The use of all the converters follows a similar approach.  First, one
 * creates a ConversionProperties object and calls
 * ConversionProperties::addOption(@if java ConversionOption option@endif)
 * on this object with one arguments: a text string that identifies the desired
 * converter.  (The text string is specific to each converter; consult the
 * documentation for a given converter to find out how it should be enabled.)
 *
 * Next, for some converters, the caller can optionally set some
 * converter-specific properties using additional calls to
 * ConversionProperties::addOption(@if java ConversionOption option@endif).
 * Many converters provide the ability to
 * configure their behavior to some extent; this is realized through the use
 * of properties that offer different options.  The default property values
 * for each converter can be interrogated using the method
 * SBMLConverter::getDefaultProperties() on the converter class in question .
 *
 * Finally, the caller should invoke the method
 * SBMLDocument::convert(@if java ConversionProperties props@endif)
 * with the ConversionProperties object as an argument.
 *
 * @subsection converter-example Example of invoking an SBML converter
 *
 * The following code fragment illustrates an example using
 * SBMLReactionConverter, which is invoked using the option string @c
 * "replaceReactions":
 *
 * @verbatim
ConversionProperties props;
props.addOption("replaceReactions");
@endverbatim
 * In the case of SBMLReactionConverter, there are no options to affect
 * its behavior, so the next step is simply to invoke the converter on
 * an SBMLDocument object.  This is also simple to do:
 *
 * @verbatim
// Assume that the variable "document" has been set to an SBMLDocument object.
int success = document->convert(props);
if (success != LIBSBML_OPERATION_SUCCESS)
{
    cerr << "Unable to perform conversion due to the following:" << endl;
    document->printErrors(cerr);
}
@endverbatim
 * Here is an example of using a converter that offers an option. The
 * following code invokes SBMLStripPackageConverter to remove the
 * SBML Level&nbsp;3 "Layout" package from a model.  It sets the name
 * of the package to be removed by adding a value for the option named
 * @c "package" defined by that converter:
 * @verbatim
ConversionProperties props;
props.addOption("stripPackage");
props.addOption("package", "layout");

int success = document->convert(props);
if (success != LIBSBML_OPERATION_SUCCESS)
{
    cerr << "Unable to strip the Layout package from the model";
    cerr << "Error returned: " << success;
}
@endverbatim
 *
 * @subsection available-converters Available SBML converters in libSBML
 *
 * LibSBML provides a number of built-in converters; by convention, their
 * names end in @em Converter. The following are the built-in converters
 * provided by libSBML @htmlinclude libsbml-version.html:
 *
 * @copydetails doc_list_of_libsbml_converters
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_formulaunitsdata
 *
 * @par The first element of the list of FormulaUnitsData refers to the default
 * units of <em>"substance per time"</em> derived from the Model object,
 * and has a <code>unitReferenceId</code> attribute of
 * 'subs_per_time'. This facilitates the comparison of units derived from
 * mathematical formula with the expected units.  The next elements of the
 * list record the units of the compartments and species established from
 * either explicitly declared or default units.  Following those, the list
 * contains the units of any parameters in the model.  Finally, subsequent
 * elements of the list record the units derived for each mathematical
 * expression encountered within the model.
 */
