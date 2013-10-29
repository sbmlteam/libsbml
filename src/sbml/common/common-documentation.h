/**
 * @file    common-documentation.h
 * @brief   Common text fragments used throughout libSBML's code documentation.
 * @author  Mike Hucka
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
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
 * The various ListOf___ classes in SBML are merely containers used for
 * organizing the main components of an SBML model.  All are derived from
 * the abstract class SBase, and inherit the various attributes and
 * subelements of SBase, such as "metaid" as and "annotation".  The
 * ListOf___ classes do not add any attributes of their own.
 *
 * The relationship between the lists and the rest of an SBML model is
 * illustrated by the following (for SBML Level&nbsp;2 Version&nbsp;4):
 *
 * @htmlinclude listof-illustration.html
 *
 * Readers may wonder about the motivations for using the ListOf___
 * containers.  A simpler approach in XML might be to place the components
 * all directly at the top level of the model definition.  The choice made
 * in SBML is to group them within XML elements named after
 * ListOf<em>Classname</em>, in part because it helps organize the
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
 * by SBML&mdash;something to be aware of if your application-specific XML
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
 * SId    ::= ( letter | '_' ) idChar*
 * </pre>
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
 * to which a given object belongs, call getPackageName().
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
 * SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif); The
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
 * the number could be stored in a @c double data type.  This is done
 * so that when an SBML model is read in and then written out again, the
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
 * For many applications, the details of ASTs are irrelevant because the
 * applications can use the text-string based translation functions such as
 * SBML_formulaToString(), SBML_parseL3Formula() and SBML_parseFormula().  If
 * you find the complexity of using the AST representation of expressions too
 * high for your purposes, perhaps the string-based functions will be more
 * suitable.
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
 * @link ASTNode.h::ASTNodeType_t <code>ASTNodeType_t</code> @endlink.@endif
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
 * node's type will be @link ASTNodeType_t#AST_PLUS AST_PLUS@endlink, @link
 * ASTNodeType_t#AST_MINUS AST_MINUS@endlink, @link ASTNodeType_t#AST_TIMES
 * AST_TIMES@endlink, @link ASTNodeType_t#AST_DIVIDE AST_DIVIDE@endlink, or
 * @link ASTNodeType_t#AST_POWER AST_POWER@endlink, as appropriate.
 *
 * @li If the node is a predefined function or operator from %SBML
 * Level&nbsp;1 (in the string-based formula syntax used in Level&nbsp;1) or
 * %SBML Level&nbsp;2 and&nbsp;3 (in the subset of MathML used in SBML
 * Levels&nbsp;2 and&nbsp;3), then the node's type
 * will be either @c AST_FUNCTION_<em><span
 * class="placeholder">X</span></em>, @c AST_LOGICAL_<em><span
 * class="placeholder">X</span></em>, or @c AST_RELATIONAL_<em><span
 * class="placeholder">X</span></em>, as appropriate.  (Examples: @link
 * ASTNodeType_t#AST_FUNCTION_LOG AST_FUNCTION_LOG@endlink, @link
 * ASTNodeType_t#AST_RELATIONAL_LEQ AST_RELATIONAL_LEQ@endlink.)
 *
 * @li If the node refers to a user-defined function, the node's type will
 * be @link ASTNodeType_t#AST_FUNCTION AST_FUNCTION@endlink (because it holds the
 * name of the function).
 *
 * @li If the node is a lambda expression, its type will be @link
 * ASTNodeType_t#AST_LAMBDA AST_LAMBDA@endlink.
 * 
 * @li If the node is a predefined constant (@c "ExponentialE", @c "Pi",
 * @c "True" or @c "False"), then the node's type will be @link
 * ASTNodeType_t#AST_CONSTANT_E AST_CONSTANT_E@endlink, @link
 * ASTNodeType_t#AST_CONSTANT_PI AST_CONSTANT_PI@endlink, @link
 * ASTNodeType_t#AST_CONSTANT_TRUE AST_CONSTANT_TRUE@endlink, or @link
 * ASTNodeType_t#AST_CONSTANT_FALSE AST_CONSTANT_FALSE@endlink.
 * 
 * @li (Levels&nbsp;2 and&nbsp;3 only) If the node is the special MathML
 * csymbol @c time, the value of the node will be @link
 * ASTNodeType_t#AST_NAME_TIME AST_NAME_TIME@endlink.  (Note, however, that
 * the MathML csymbol @c delay is translated into a node of type @link
 * ASTNodeType_t#AST_FUNCTION_DELAY AST_FUNCTION_DELAY@endlink.  The
 * difference is due to the fact that @c time is a single variable, whereas
 * @c delay is actually a function taking arguments.)
 *
 * @li (Level&nbsp;3 only) If the node is the special MathML csymbol @c avogadro,
 * the value of the node will be @c AST_NAME_AVOGADRO.
 *
 * @li If the node contains a numerical value, its type will be @link
 * ASTNodeType_t#AST_INTEGER AST_INTEGER@endlink, @link
 * ASTNodeType_t#AST_REAL AST_REAL@endlink, @link ASTNodeType_t#AST_REAL_E
 * AST_REAL_E@endlink, or @link ASTNodeType_t#AST_RATIONAL
 * AST_RATIONAL@endlink, as appropriate.
 *
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_summary_of_string_math
 *
 * @par
 * The text-string form of mathematical formulas produced by
 * @if clike SBML_formulaToString()@endif@if csharp SBML_formulaToString()@endif@if python libsbml.formulaToString()@endif@if java <code><a href="libsbml.html#formulaToString(org.sbml.libsbml.ASTNode)">libsbml.formulaToString(ASTNode tree)</a></code>@endif@~
 * and read by @if clike SBML_parseFormula()@endif@if csharp SBML_parseFormula()@endif@if python libsbml.parseFormula()@endif@if java <code><a href="libsbml.html#parseFormula(java.lang.String)">libsbml.parseFormula(String formula)</a></code>@endif@~
 * use a simple C-inspired infix notation taken from SBML Level&nbsp;1.  A
 * formula in this text-string form therefore can be handed to a program
 * that understands SBML Level&nbsp;1 mathematical expressions, or used as
 * part of a formula translation system.  The syntax is described in detail
 * in the documentation for ASTNode. 
 *
 * Note that this facility is provided as a convenience by libSBML&mdash;the
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
 * The text-string form of mathematical formulas read by the functions
 * @if clike SBML_formulaToL3String()@endif@if csharp
 * SBML_formulaL3ToString()@endif@if python
 * libsbml.formulaToL3String()@endif@if java <code><a
 * href="libsbml.html#formulaToL3String(org.sbml.libsbml.ASTNode)">libsbml.formulaToL3String(ASTNode
 * tree)</a></code>@endif@~ and @if clike SBML_parseL3Formula()@endif@if csharp
 * SBML_parseL3Formula()@endif@if python
 * libsbml.parseL3Formula()@endif@if java <code><a
 * href="libsbml.html#parseFormula(java.lang.String)">libsbml.parseFormula(String
 * formula)</a></code>@endif@~ are expanded versions of the formats produced
 * and read by @if clike SBML_formulaToString()@endif@if csharp
 * SBML_formulaToString()@endif@if python libsbml.formulaToString()@endif@if java <code><a
 * href="libsbml.html#formulaToString(org.sbml.libsbml.ASTNode)">libsbml.formulaToString(ASTNode
 * tree)</a></code>@endif@~ and @if clike SBML_parseFormula()@endif@if csharp
 * SBML_parseFormula()@endif@if python libsbml.parseFormula()@endif@if java
 * <code><a
 * href="libsbml.html#parseFormula(java.lang.String)">libsbml.parseFormula(String
 * formula)</a></code>@endif@~, respectively.  The latter two libSBML
 * functions were originally developed to support conversion between SBML
 * Levels&nbsp;1 and&nbsp;2, and were focused on the syntax of mathematical
 * formulas used in SBML Level&nbsp;1.  With time, and the use of MathML in
 * SBML Levels&nbsp;2 and&nbsp;3, it became clear that supporting
 * Level&nbsp;2 and&nbsp;3's expanded mathematical syntax would be useful for
 * software developers.
 * To maintain backwards compatibility, the original
 * @if clike SBML_formulaToString()@endif@if csharp SBML_formulaToString()@endif@if python libsbml.formulaToString()@endif@if java <code><a href="libsbml.html#formulaToString(org.sbml.libsbml.ASTNode)">libsbml.formulaToString(ASTNode tree)</a></code>@endif@~
 * and
 * @if clike SBML_parseFormula()@endif@if csharp SBML_parseFormula()@endif@if python libsbml.parseFormula()@endif@if java <code><a href="libsbml.html#parseFormula(java.lang.String)">libsbml.parseFormula(String formula)</a></code>@endif@~
 * have been left untouched, and instead, the new functionality is
 * provided in the form of
 * @if clike SBML_parseL3Formula()@endif@if csharp SBML_parseL3Formula()@endif@if python libsbml.parseL3Formula()@endif@if java <cod
e><a href="libsbml.html#parseL3Formula(java.lang.String)">libsbml.parseL3Formula(String formula)</a></code>@endif@~
 * and @if clike SBML_formulaToL3String()@endif@if csharp
 * SBML_formulaL3ToString()@endif@if python
 * libsbml.formulaToL3String()@endif@if java <code><a
 * href="libsbml.html#formulaToL3String(org.sbml.libsbml.ASTNode)">libsbml.formulaToL3String(ASTNode
 * tree)</a></code>@endif@~.
 *
 * The following are the differences in the formula syntax supported by the
 * "L3" versions of the formula parsers and formatters, compared to what is
 * supported by @if clike SBML_parseFormula()@endif@if csharp
 * SBML_parseFormula()@endif@if python libsbml.parseFormula()@endif@if java
 * <code><a
 * href="libsbml.html#parseFormula(java.lang.String)">libsbml.parseFormula(String
 * formula)</a></code>@endif@~ and @if clike
 * SBML_formulaToL3String()@endif@if csharp SBML_formulaL3ToString()@endif@if python
 * libsbml.formulaToL3String()@endif@if java <code><a
 * href="libsbml.html#formulaToL3String(org.sbml.libsbml.ASTNode)">libsbml.formulaToL3String(ASTNode
 * tree)</a></code>@endif@~:
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
 * produce a piecewise function in the MathML.
 *
 * @li All inverse trigonometric functions may be defined in the infix either
 * using @c arc as a prefix or simply @c a; in other words, both @c arccsc
 * and @c acsc are interpreted as the operator @em arccosecant defined in
 * MathML.  (Many functions in the SBML Level&nbsp;1 infix-notation parser
 * implemented by @if clike SBML_parseFormula()@endif@if csharp
 * SBML_parseFormula()@endif@if python libsbml.parseFormula()@endif@if java
 * <code><a
 * href="libsbml.html#parseFormula(java.lang.String)">libsbml.parseFormula(String
 * formula)</a></code>@endif@~ are defined this way as well, but not all.)
 *
 * @li The following expression is parsed as a rational number instead of
 * as a numerical division:
 * <pre style="display: block; margin-left: 25px">
 * (<span class="code" style="background-color: #d0d0ee">integer</span>/<span class="code" style="background-color: #d0d0ee">integer</span>)</pre>
 * No spaces are allowed in this construct; in other words,
 * &quot;<code>(3 / 4)</code>&quot; will be parsed into the MathML
 * <code>&lt;divide&gt;</code> construct rather than a rational number.  The 
 * general number syntax allows you to assign units to a rational number, e.g.,
 * &quot;<code>(3/4) ml</code>&quot;.  (If the string is a division, units
 * are not interpreted in this way.)
 *
 * @li Various settings may be altered by using an L3ParserSettings object in
 * conjunction with the functions @if clike
 * SBML_parseL3FormulaWithSettings()@endif@if csharp
 * SBML_parseL3FormulaWithSettings()@endif@if python
 * libsbml.parseL3FormulaWithSettings()@endif@if java <code><a
 * href="libsbml.html#parseL3FormulaWithSettings(java.lang.String,
 * org.sbml.libsbml.L3ParserSettings)">libsbml.parseL3FormulaWithSettings(String
 * formula, L3ParserSettings settings)</a></code>@endif@~ and @if clike
 * SBML_formulaToL3String()@endif@if csharp SBML_formulaL3ToString()@endif@if python
 * libsbml.formulaToL3String()@endif@if java <code><a
 * href="libsbml.html#formulaToL3String(org.sbml.libsbml.ASTNode)">libsbml.formulaToL3String(ASTNode
 * tree)</a></code>@endif@~, including the following:
 * <ul>
 * <li> The function @c log with a single argument (&quot;<code>log(x)</code>&quot;) 
 * can be parsed as <code>log10(x)</code>, <code>ln(x)</code>, or treated
 * as an error, as desired.
 * <li> Unary minus signs can be collapsed or preserved; that is,
 * sequential pairs of unary minuses (e.g., &quot;<code>- -3</code>&quot;)
 * can be removed from the input entirely and single unary minuses can be
 * incorporated into the number node, or all minuses can be preserved in
 * the AST node structure.
 * <li> Parsing of units embedded in the input string can be turned on and
 * off.
 * <li> The string @c avogadro can be parsed as a MathML @em csymbol or
 * as an identifier.
 * <li> A Model object may optionally be provided to the parser using the
 * variant function call
 * @if clike SBML_parseL3FormulaWithModel()@endif@if csharp
 * SBML_parseL3FormulaWithModel()@endif@if python
 * libsbml.SBML_parseL3FormulaWithModel()@endif@if java <code><a
 * href="libsbml.html#parseL3FormulaWithModel(java.lang.String,
 * org.sbml.libsbml.Model)">libsbml.parseL3FormulaWithModel(String formula,
 * Model model)</a></code>@endif@~ or stored in a L3ParserSettings object
 * passed to the variant function @if clike
 * SBML_parseL3FormulaWithSettings()@endif@if csharp
 * SBML_parseL3FormulaWithSettings()@endif@if python
 * libsbml.parseL3FormulaWithSettings()@endif@if java <code><a
 * href="libsbml.html#parseL3FormulaWithSettings(java.lang.String,
 * org.sbml.libsbml.L3ParserSettings)">libsbml.parseL3FormulaWithSettings(String
 * formula, L3ParserSettings settings)</a></code>@endif@~.
 * When a Model object is provided, identifiers (values of type @c SId) from
 * that model are used in preference to pre-defined MathML definitions.  More
 * precisely, the Model entities whose identifiers will shadow identical
 * symbols in the mathematical formula are: Species, Compartment, Parameter,
 * Reaction, and SpeciesReference.  For instance, if the parser is given a
 * Model containing a Species with the identifier
 * &quot;<code>pi</code>&quot;, and the formula to be parsed is
 * &quot;<code>3*pi</code>&quot;, the MathML produced will contain the
 * construct <code>&lt;ci&gt; pi &lt;/ci&gt;</code> instead of the construct
 * <code>&lt;pi/&gt;</code>.  <li> Similarly, when a Model object is
 * provided, @c SId values of user-defined functions present in the model
 * will be used preferentially over pre-defined MathML functions.  For
 * example, if the passed-in Model contains a FunctionDefinition with the
 * identifier &quot;<code>sin</code>&quot;, that function will be used
 * instead of the predefined MathML function <code>&lt;sin/&gt;</code>.
 * </ul>
 * These configuration settings cannot be changed using the basic parser and
 * formatter functions, but can be changed on a per-call basis by using the
 * alternative functions @if clike
 * SBML_parseL3FormulaWithSettings()@endif@if csharp
 * SBML_parseL3FormulaWithSettings()@endif@if python
 * libsbml.parseL3FormulaWithSettings()@endif@if java <code><a
 * href="libsbml.html#parseL3FormulaWithSettings(java.lang.String,
 * org.sbml.libsbml.L3ParserSettings)">libsbml.parseL3FormulaWithSettings(String
 * formula, L3ParserSettings settings)</a></code>@endif@~ and @if clike
 * SBML_formulaToL3StringWithSettings()@endif@if csharp
 * SBML_formulaToL3StringWithSettings()@endif@if python
 * libsbml.formulaToL3StringWithSettings()@endif@if java <code><a
 * href="libsbml.html#formulaToL3StringWithSettings(const ASTNode_t tree,
 * org.sbml.libsbml.L3ParserSettings)">libsbml.parseL3FormulaWithSettings(ASTNode tree
 * L3ParserSettings settings)</a></code>@endif@~.
 *
 * The parser function @if clike
 * SBML_parseL3FormulaWithSettings()@endif@if csharp
 * SBML_parseL3FormulaWithSettings()@endif@if python
 * libsbml.parseL3FormulaWithSettings()@endif@if java <code><a
 * href="libsbml.html#parseL3FormulaWithSettings(java.lang.String,
 * org.sbml.libsbml.L3ParserSettings)">libsbml.parseL3FormulaWithSettings(String
 * formula, L3ParserSettings settings)</a></code>@endif@~
 * returns the root node of the AST corresponding to the
 * formula given as the argument.  If the formula contains a syntax error,
 * the function will return @c NULL instead.  When @c NULL is returned, an
 * error is set; information about the error can be retrieved using
 * @if clike SBML_getLastParseL3Error()@endif@if csharp SBML_getLastParseL3Error()@endif@if python libsbml.getLastParseL3Error()@endif@if java <code><a href="libsbml.html#getLastParseL3Error()">libsbml.getLastParseL3Error()</a></code>@endif@~.
 *
 * Note that this facility and the SBML Level&nbsp;1-based @if clike
 * SBML_parseFormula()@endif@if csharp SBML_parseFormula()@endif@if python
 * libsbml.parseFormula()@endif@if java <code><a
 * href="libsbml.html#parseFormula(java.lang.String)">libsbml.parseFormula(String
 * formula)</a></code>@endif@~ are provided as a convenience by
 * libSBML&mdash;the MathML standard does not actually define a "string-form"
 * equivalent to MathML expressions, so the choice of formula syntax is
 * arbitrary.  The approach taken by libSBML is to start with the syntax
 * defined by SBML Level&nbsp;1 (which in fact used a text-string
 * representation of formulas, and not MathML), and expand it to include the
 * above functionality.  This formula syntax is based mostly on C programming
 * syntax, and may contain operators, function calls, symbols, and white
 * space characters.  The following table provides the precedence rules for
 * the different entities that may appear in formula strings.
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
 * Note that the manner in which the "L3" versions of the formula parser and
 * formatter interpret the function &quot;<code>log</code>&quot; can be
 * changed.  To do so, callers should use the function @if clike
 * SBML_parseL3FormulaWithSettings()@endif@if csharp
 * SBML_parseL3FormulaWithSettings()@endif@if python
 * libsbml.parseL3FormulaWithSettings()@endif@if java <code><a
 * href="libsbml.html#parseL3FormulaWithSettings(java.lang.String,
 * org.sbml.libsbml.L3ParserSettings)">libsbml.parseL3FormulaWithSettings(String
 * formula, L3ParserSettings settings)</a></code>@endif@~ 
 * and pass it an appropriate L3ParserSettings object.  By default,
 * unlike the SBML Level&nbsp;1 parser implemented by @if clike
 * SBML_parseFormula()@endif@if csharp SBML_parseFormula()@endif@if python
 * libsbml.parseFormula()@endif@if java <code><a
 * href="libsbml.html#parseFormula(java.lang.String)">libsbml.parseFormula(String
 * formula)</a></code>@endif@~, the string &quot;<code>log</code>&quot; is
 * interpreted as the base&nbsp;10 logarithm, and @em not as the natural
 * logarithm.  However, you can change the interpretation to be base-10 log,
 * natural log, or as an error; since the name "log" by itself is ambiguous,
 * you require that the parser uses @c log10 or @c ln instead, which are more
 * clear.  Please refer to @if clike
 * SBML_parseL3FormulaWithSettings()@endif@if csharp
 * SBML_parseL3FormulaWithSettings()@endif@if python
 * libsbml.parseL3FormulaWithSettings()@endif@if java <code><a
 * href="libsbml.html#parseL3FormulaWithSettings(java.lang.String,
 * org.sbml.libsbml.L3ParserSettings)">libsbml.parseL3FormulaWithSettings(String
 * formula, L3ParserSettings settings)</a></code>@endif@~.
 * 
 * In addition, the following symbols will be translated to their MathML
 * equivalents, if no symbol with the same @c SId identifier string exists
 * in the Model object provided:
 *
 * @htmlinclude string-values-table-l3.html
 * 
 * Note that whether the string &quot;<code>avogadro</code>&quot; is parsed
 * as an AST node of type @link ASTNodeType_t#AST_NAME_AVOGADRO
 * AST_NAME_AVOGADRO@endlink or @link ASTNodeType_t#AST_NAME AST_NAME@endlink
 * is configurable; use the alternate version of this function, called @if clike
 * SBML_parseL3FormulaWithSettings()@endif@if csharp
 * SBML_parseL3FormulaWithSettings()@endif@if python
 * libsbml.parseL3FormulaWithSettings()@endif@if java <code><a
 * href="libsbml.html#parseL3FormulaWithSettings(java.lang.String,
 * org.sbml.libsbml.L3ParserSettings)">libsbml.parseL3FormulaWithSettings(String
 * formula, L3ParserSettings settings)</a></code>@endif@~.  This
 * functionality is provided because SBML Level&nbsp;2 models may not use
 * @link ASTNodeType_t#AST_NAME_AVOGADRO AST_NAME_AVOGADRO@endlink AST nodes.
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
 * assignment statements must not contain algebraic loops&mdash;dependency
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
 * @li @link RuleType_t#RULE_TYPE_RATE RULE_TYPE_RATE@endlink: Indicates
 * the rule is a "rate" rule.
 * @li @link RuleType_t#RULE_TYPE_SCALAR RULE_TYPE_SCALAR@endlink:
 * Indicates the rule is a "scalar" rule.
 * @li @link RuleType_t#RULE_TYPE_INVALID RULE_TYPE_INVALID@endlink:
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
 * @class doc_warning_L1_math_string_syntax
 * 
 * @warning <span class="warning">We urge developers to keep in mind that
 * the text-string formula syntax is specific to SBML Level&nbsp;1's C-like
 * mathematical formula syntax.  In particular, it is <em>not a
 * general-purpose mathematical expression syntax</em>.  LibSBML provides
 * methods for parsing and transforming text-string math formulas back and
 * forth from AST structures, but it is important to keep the system's
 * limitations in mind.</span>
 * 
 */
