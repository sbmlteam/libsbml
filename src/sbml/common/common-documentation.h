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
 * the number could be stored in a
 * @if python @c float@endif@if clike @c double @endif data type.  This is
 * done so that when an SBML model is read in and then written out again, the
 * amount of change introduced by libSBML to the SBML during the round-trip
 * activity is minimized.
 *  
 * @li Rational numbers are represented in an AST node using separate
 * numerator and denominator values.  These can be retrieved using the
 * methods @if cpp ASTNode::getNumerator() and ASTNode::getDenominator().
 * @endif@if python
 * @link libsbml.ASTNode.getNumerator() ASTNode.getNumerator()@endlink and
 * @link libsbml.ASTNode.getDenominator() ASTNode.getDenominator()@endlink.
 * @endif
 * 
 * @li The children of an ASTNode are other ASTNode objects.  The list of
 * children is empty for nodes that are leaf elements, such as numbers.
 * For nodes that are actually roots of expression subtrees, the list of
 * children points to the parsed objects that make up the rest of the
 * expression.
 *
 * For many applications, the details of ASTs are irrelevant because the
 * applications can use the text-string based translation functions such as
 * @if clike SBML_formulaToString(), SBML_parseL3Formula() and
 * SBML_parseFormula()@endif@if python libsbml.formulaToString(),
 * libsbml.parseL3Formula() and libsbml.parseFormula()@endif.  If
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
<table width="95%" cellspacing="1" cellpadding="2" border="0"
       class="centered text-table small-font alt-row-colors">
 <tr style="background: lightgray" class="normal-font">
     <th valign="bottom"><strong>Enumerator</strong></th>
     <th valign="bottom"><strong>Meaning</strong></th>
     <th align="center" width="15">L 1 V 1</th>
     <th align="center" width="15">L 1 V 2</th>
     <th align="center" width="15">L 2 V 1</th>
     <th align="center" width="15">L 2 V 2</th>
     <th align="center" width="15">L 2 V 3</th>
     <th align="center" width="15">L 2 V 4</th>
     <th align="center" width="15">L 3 V 1</th>
 </tr>
<tr><td><code>@link SBMLErrorCode_t#UnknownError UnknownError @endlink</code></td>
<td>Encountered unknown internal libSBML error</td>
<td class="s-fatal">F</td>
<td class="s-fatal">F</td>
<td class="s-fatal">F</td>
<td class="s-fatal">F</td>
<td class="s-fatal">F</td>
<td class="s-fatal">F</td>
<td class="s-fatal">F</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NotUTF8 NotUTF8 @endlink</code></td>
<td>File does not use UTF-8 encoding</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#UnrecognizedElement UnrecognizedElement @endlink</code></td>
<td>Encountered unrecognized element</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NotSchemaConformant NotSchemaConformant @endlink</code></td>
<td>Document does not conform to the SBML XML schema</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#L3NotSchemaConformant L3NotSchemaConformant @endlink</code></td>
<td>Document is not well-formed XML</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidMathElement InvalidMathElement @endlink</code></td>
<td>Invalid MathML</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DisallowedMathMLSymbol DisallowedMathMLSymbol @endlink</code></td>
<td>Disallowed MathML symbol found</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DisallowedMathMLEncodingUse DisallowedMathMLEncodingUse @endlink</code></td>
<td>Use of the MathML 'encoding' attribute is not allowed on this element</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DisallowedDefinitionURLUse DisallowedDefinitionURLUse @endlink</code></td>
<td>Use of the MathML 'definitionURL' attribute is not allowed on this element</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#BadCsymbolDefinitionURLValue BadCsymbolDefinitionURLValue @endlink</code></td>
<td>Invalid <code>&lt;csymbol&gt;</code> 'definitionURL' attribute value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DisallowedMathTypeAttributeUse DisallowedMathTypeAttributeUse @endlink</code></td>
<td>Use of the MathML 'type' attribute is not allowed on this element</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DisallowedMathTypeAttributeValue DisallowedMathTypeAttributeValue @endlink</code></td>
<td>Disallowed MathML 'type' attribute value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#LambdaOnlyAllowedInFunctionDef LambdaOnlyAllowedInFunctionDef @endlink</code></td>
<td>Use of <code>&lt;lambda&gt;</code> not permitted outside of FunctionDefinition objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#BooleanOpsNeedBooleanArgs BooleanOpsNeedBooleanArgs @endlink</code></td>
<td>Non-Boolean argument given to Boolean operator</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NumericOpsNeedNumericArgs NumericOpsNeedNumericArgs @endlink</code></td>
<td>Non-numerical argument given to numerical operator</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ArgsToEqNeedSameType ArgsToEqNeedSameType @endlink</code></td>
<td>Arguments to <code>&lt;eq&gt;</code> and <code>&lt;neq&gt;</code> must have the same data types</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#PiecewiseNeedsConsistentTypes PiecewiseNeedsConsistentTypes @endlink</code></td>
<td>Terms in a <code>&lt;piecewise&gt;</code> expression must have consistent data types</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#PieceNeedsBoolean PieceNeedsBoolean @endlink</code></td>
<td>The second argument of a <code>&lt;piece&gt;</code> expression must yield a Boolean value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ApplyCiMustBeUserFunction ApplyCiMustBeUserFunction @endlink</code></td>
<td>A <code>&lt;ci&gt;</code> element in this context must refer to a function definition</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ApplyCiMustBeModelComponent ApplyCiMustBeModelComponent @endlink</code></td>
<td>A <code>&lt;ci&gt;</code> element in this context must refer to a model component</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#KineticLawParametersAreLocalOnly KineticLawParametersAreLocalOnly @endlink</code></td>
<td>Cannot use a KineticLaw local parameter outside of its local scope</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MathResultMustBeNumeric MathResultMustBeNumeric @endlink</code></td>
<td>A formula's result in this context must be a numerical value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OpsNeedCorrectNumberOfArgs OpsNeedCorrectNumberOfArgs @endlink</code></td>
<td>Incorrect number of arguments given to MathML operator</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidNoArgsPassedToFunctionDef InvalidNoArgsPassedToFunctionDef @endlink</code></td>
<td>Incorrect number of arguments given to function invocation</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DisallowedMathUnitsUse DisallowedMathUnitsUse @endlink</code></td>
<td>Attribute 'units' is only permitted on <code>&lt;cn&gt;</code> elements</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidUnitsValue InvalidUnitsValue @endlink</code></td>
<td>Invalid value given for the 'units' attribute</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DuplicateComponentId DuplicateComponentId @endlink</code></td>
<td>Duplicate 'id' attribute value</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DuplicateUnitDefinitionId DuplicateUnitDefinitionId @endlink</code></td>
<td>Duplicate unit definition 'id' attribute value</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DuplicateLocalParameterId DuplicateLocalParameterId @endlink</code></td>
<td>Duplicate local parameter 'id' attribute value</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MultipleAssignmentOrRateRules MultipleAssignmentOrRateRules @endlink</code></td>
<td>Multiple rules for the same variable are not allowed</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MultipleEventAssignmentsForId MultipleEventAssignmentsForId @endlink</code></td>
<td>Multiple event assignments for the same variable are not allowed</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#EventAndAssignmentRuleForId EventAndAssignmentRuleForId @endlink</code></td>
<td>An event assignment and an assignment rule must not have the same value for 'variable'</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DuplicateMetaId DuplicateMetaId @endlink</code></td>
<td>Duplicate 'metaid' attribute value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidSBOTermSyntax InvalidSBOTermSyntax @endlink</code></td>
<td>Invalid syntax for an 'sboTerm' attribute value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidMetaidSyntax InvalidMetaidSyntax @endlink</code></td>
<td>Invalid syntax for a 'metaid' attribute value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidIdSyntax InvalidIdSyntax @endlink</code></td>
<td>Invalid syntax for an 'id' attribute value</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidUnitIdSyntax InvalidUnitIdSyntax @endlink</code></td>
<td>Invalid syntax for the identifier of a unit</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidNameSyntax InvalidNameSyntax @endlink</code></td>
<td>Invalid syntax for a 'name' attribute value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MissingAnnotationNamespace MissingAnnotationNamespace @endlink</code></td>
<td>Missing declaration of the XML namespace for the annotation</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DuplicateAnnotationNamespaces DuplicateAnnotationNamespaces @endlink</code></td>
<td>Multiple annotations using the same XML namespace</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SBMLNamespaceInAnnotation SBMLNamespaceInAnnotation @endlink</code></td>
<td>The SBML XML namespace cannot be used in an Annotation object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MultipleAnnotations MultipleAnnotations @endlink</code></td>
<td>Only one Annotation object is permitted under a given SBML object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InconsistentArgUnits InconsistentArgUnits @endlink</code></td>
<td>The units of the function call's arguments are not consistent with its definition</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InconsistentKineticLawUnitsL3 InconsistentKineticLawUnitsL3 @endlink</code></td>
<td>The kinetic law's units are inconsistent with those of other kinetic laws in the model</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AssignRuleCompartmentMismatch AssignRuleCompartmentMismatch @endlink</code></td>
<td>Mismatched units in assignment rule for compartment</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AssignRuleSpeciesMismatch AssignRuleSpeciesMismatch @endlink</code></td>
<td>Mismatched units in assignment rule for species</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AssignRuleParameterMismatch AssignRuleParameterMismatch @endlink</code></td>
<td>Mismatched units in assignment rule for parameter</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AssignRuleStoichiometryMismatch AssignRuleStoichiometryMismatch @endlink</code></td>
<td>Mismatched units in assignment rule for stoichiometry</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InitAssignCompartmenMismatch InitAssignCompartmenMismatch @endlink</code></td>
<td>Mismatched units in initial assignment to compartment</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InitAssignSpeciesMismatch InitAssignSpeciesMismatch @endlink</code></td>
<td>Mismatched units in initial assignment to species</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InitAssignParameterMismatch InitAssignParameterMismatch @endlink</code></td>
<td>Mismatched units in initial assignment to parameter</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InitAssignStoichiometryMismatch InitAssignStoichiometryMismatch @endlink</code></td>
<td>Mismatched units in initial assignment to stoichiometry</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#RateRuleCompartmentMismatch RateRuleCompartmentMismatch @endlink</code></td>
<td>Mismatched units in rate rule for compartment</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#RateRuleSpeciesMismatch RateRuleSpeciesMismatch @endlink</code></td>
<td>Mismatched units in rate rule for species</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#RateRuleParameterMismatch RateRuleParameterMismatch @endlink</code></td>
<td>Mismatched units in rate rule for parameter</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#RateRuleStoichiometryMismatch RateRuleStoichiometryMismatch @endlink</code></td>
<td>Mismatched units in rate rule for stoichiometry</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#KineticLawNotSubstancePerTime KineticLawNotSubstancePerTime @endlink</code></td>
<td>The units of the kinetic law are not 'substance'/'time'</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SpeciesInvalidExtentUnits SpeciesInvalidExtentUnits @endlink</code></td>
<td>The species' units are not consistent with units of extent</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DelayUnitsNotTime DelayUnitsNotTime @endlink</code></td>
<td>The units of the delay expression are not units of time</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#EventAssignCompartmentMismatch EventAssignCompartmentMismatch @endlink</code></td>
<td>Mismatched units in event assignment for compartment</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#EventAssignSpeciesMismatch EventAssignSpeciesMismatch @endlink</code></td>
<td>Mismatched units in event assignment for species</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#EventAssignParameterMismatch EventAssignParameterMismatch @endlink</code></td>
<td>Mismatched units in event assignment for parameter</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#EventAssignStoichiometryMismatch EventAssignStoichiometryMismatch @endlink</code></td>
<td>Mismatched units in event assignment for stoichiometry</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#PriorityUnitsNotDimensionless PriorityUnitsNotDimensionless @endlink</code></td>
<td>The units of a priority expression must be 'dimensionless'</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#UpperUnitBound UpperUnitBound @endlink</code></td>
<td>Upper boundary of unit validation diagnostic codes</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OverdeterminedSystem OverdeterminedSystem @endlink</code></td>
<td>The model is overdetermined</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidModelSBOTerm InvalidModelSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for a Model object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidFunctionDefSBOTerm InvalidFunctionDefSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for a FunctionDefinition object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidParameterSBOTerm InvalidParameterSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for a Parameter object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidInitAssignSBOTerm InvalidInitAssignSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for an InitialAssignment object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidRuleSBOTerm InvalidRuleSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for a Rule object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidConstraintSBOTerm InvalidConstraintSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for a Constraint object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidReactionSBOTerm InvalidReactionSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for a Reaction object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidSpeciesReferenceSBOTerm InvalidSpeciesReferenceSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for a SpeciesReference object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidKineticLawSBOTerm InvalidKineticLawSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for a KineticLaw object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidEventSBOTerm InvalidEventSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for an Event object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidEventAssignmentSBOTerm InvalidEventAssignmentSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for an EventAssignment object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidCompartmentSBOTerm InvalidCompartmentSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for a Compartment object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidSpeciesSBOTerm InvalidSpeciesSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for a Species object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidCompartmentTypeSBOTerm InvalidCompartmentTypeSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for a CompartmentType object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidSpeciesTypeSBOTerm InvalidSpeciesTypeSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for a SpeciesType object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidTriggerSBOTerm InvalidTriggerSBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for an Event Trigger object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidDelaySBOTerm InvalidDelaySBOTerm @endlink</code></td>
<td>Invalid 'sboTerm' attribute value for an Event Delay object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NotesNotInXHTMLNamespace NotesNotInXHTMLNamespace @endlink</code></td>
<td>Notes must be placed in the XHTML XML namespace</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NotesContainsXMLDecl NotesContainsXMLDecl @endlink</code></td>
<td>XML declarations are not permitted in Notes objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NotesContainsDOCTYPE NotesContainsDOCTYPE @endlink</code></td>
<td>XML DOCTYPE elements are not permitted in Notes objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidNotesContent InvalidNotesContent @endlink</code></td>
<td>Invalid notes content found</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyOneNotesElementAllowed OnlyOneNotesElementAllowed @endlink</code></td>
<td>Only one Notes subobject is permitted on a given SBML object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidNamespaceOnSBML InvalidNamespaceOnSBML @endlink</code></td>
<td>Invalid XML namespace for the SBML container element</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MissingOrInconsistentLevel MissingOrInconsistentLevel @endlink</code></td>
<td>Missing or inconsistent value for the 'level' attribute</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MissingOrInconsistentVersion MissingOrInconsistentVersion @endlink</code></td>
<td>Missing or inconsistent value for the 'version' attribute</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#PackageNSMustMatch PackageNSMustMatch @endlink</code></td>
<td>Inconsistent or invalid SBML Level/Version for the package namespace declaration</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#LevelPositiveInteger LevelPositiveInteger @endlink</code></td>
<td>The 'level' attribute must have a positive integer value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#VersionPositiveInteger VersionPositiveInteger @endlink</code></td>
<td>The 'version' attribute must have a positive integer value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnSBML AllowedAttributesOnSBML @endlink</code></td>
<td>Invalid attribute found on the SBML container element</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#L3PackageOnLowerSBML L3PackageOnLowerSBML @endlink</code></td>
<td>An L3 package ns found on the SBML container element.</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MissingModel MissingModel @endlink</code></td>
<td>No model definition found</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#IncorrectOrderInModel IncorrectOrderInModel @endlink</code></td>
<td>Incorrect ordering of components within the Model object</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#EmptyListElement EmptyListElement @endlink</code></td>
<td>Empty ListOf___ object found</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NeedCompartmentIfHaveSpecies NeedCompartmentIfHaveSpecies @endlink</code></td>
<td>The presence of a species requires a compartment</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneOfEachListOf OneOfEachListOf @endlink</code></td>
<td>Only one of each kind of ListOf___ object is allowed inside a Model object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyFuncDefsInListOfFuncDefs OnlyFuncDefsInListOfFuncDefs @endlink</code></td>
<td>Only FunctionDefinition, Notes and Annotation objects are allowed in ListOfFunctionDefinitions</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyUnitDefsInListOfUnitDefs OnlyUnitDefsInListOfUnitDefs @endlink</code></td>
<td>Only UnitDefinition, Notes and Annotation objects are allowed in ListOfUnitDefinitions objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyCompartmentsInListOfCompartments OnlyCompartmentsInListOfCompartments @endlink</code></td>
<td>Only Compartment, Notes and Annotation objects are allowed in ListOfCompartments objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlySpeciesInListOfSpecies OnlySpeciesInListOfSpecies @endlink</code></td>
<td>Only Species, Notes and Annotation objects are allowed in ListOfSpecies objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyParametersInListOfParameters OnlyParametersInListOfParameters @endlink</code></td>
<td>Only Parameter, Notes and Annotation objects are allowed in ListOfParameters objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyInitAssignsInListOfInitAssigns OnlyInitAssignsInListOfInitAssigns @endlink</code></td>
<td>Only InitialAssignment, Notes and Annotation objects are allowed in ListOfInitialAssignments objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyRulesInListOfRules OnlyRulesInListOfRules @endlink</code></td>
<td>Only Rule, Notes and Annotation objects are allowed in ListOfRules objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyConstraintsInListOfConstraints OnlyConstraintsInListOfConstraints @endlink</code></td>
<td>Only Constraint, Notes and Annotation objects are allowed in ListOfConstraints objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyReactionsInListOfReactions OnlyReactionsInListOfReactions @endlink</code></td>
<td>Only Reaction, Notes and Annotation objects are allowed in ListOfReactions objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyEventsInListOfEvents OnlyEventsInListOfEvents @endlink</code></td>
<td>Only Event, Notes and Annotation objects are allowed in ListOfEvents objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#L3ConversionFactorOnModel L3ConversionFactorOnModel @endlink</code></td>
<td>A 'conversionFactor' attribute value must reference a Parameter object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#L3TimeUnitsOnModel L3TimeUnitsOnModel @endlink</code></td>
<td>Invalid 'timeUnits' attribute value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#L3VolumeUnitsOnModel L3VolumeUnitsOnModel @endlink</code></td>
<td>Invalid 'volumeUnits' attribute value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#L3AreaUnitsOnModel L3AreaUnitsOnModel @endlink</code></td>
<td>Invalid 'areaUnits' attribute value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#L3LengthUnitsOnModel L3LengthUnitsOnModel @endlink</code></td>
<td>Invalid 'lengthUnits' attribute value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#L3ExtentUnitsOnModel L3ExtentUnitsOnModel @endlink</code></td>
<td>Invalid 'extentUnits' attribute value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnModel AllowedAttributesOnModel @endlink</code></td>
<td>Invalid attribute found on the Model object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfFuncs AllowedAttributesOnListOfFuncs @endlink</code></td>
<td>Invalid attribute found on the ListOfFunctionDefinitions object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfUnitDefs AllowedAttributesOnListOfUnitDefs @endlink</code></td>
<td>Invalid attribute found on the ListOfUnitDefinitions object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfComps AllowedAttributesOnListOfComps @endlink</code></td>
<td>Invalid attribute found on the ListOfCompartments object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfSpecies AllowedAttributesOnListOfSpecies @endlink</code></td>
<td>Invalid attribute found on the ListOfSpecies object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfParams AllowedAttributesOnListOfParams @endlink</code></td>
<td>Invalid attribute found on the ListOfParameters object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfInitAssign AllowedAttributesOnListOfInitAssign @endlink</code></td>
<td>Invalid attribute found on the ListOfInitialAssignments object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfRules AllowedAttributesOnListOfRules @endlink</code></td>
<td>Invalid attribute found on the ListOfRules object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfConstraints AllowedAttributesOnListOfConstraints @endlink</code></td>
<td>Invalid attribute found on the ListOfConstraints object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfReactions AllowedAttributesOnListOfReactions @endlink</code></td>
<td>Invalid attribute found on the ListOfReactions object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfEvents AllowedAttributesOnListOfEvents @endlink</code></td>
<td>Invalid attribute found on the ListOfEvents object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#FunctionDefMathNotLambda FunctionDefMathNotLambda @endlink</code></td>
<td>Invalid expression found in the function definition</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidApplyCiInLambda InvalidApplyCiInLambda @endlink</code></td>
<td>Invalid forward reference in the MathML <code>&lt;apply&gt;</code><code>&lt;ci&gt;</code>...<code>&lt;/ci&gt;</code><code>&lt;/apply&gt;</code> expression</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#RecursiveFunctionDefinition RecursiveFunctionDefinition @endlink</code></td>
<td>Recursive function definitions are not permitted</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidCiInLambda InvalidCiInLambda @endlink</code></td>
<td>Invalid <code>&lt;ci&gt;</code> reference found inside the <code>&lt;lambda&gt;</code> mathematical formula</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidFunctionDefReturnType InvalidFunctionDefReturnType @endlink</code></td>
<td>A function's return type must be either a number or a Boolean</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneMathElementPerFunc OneMathElementPerFunc @endlink</code></td>
<td>A FunctionDefinition object must contain one <code>&lt;math&gt;</code> element</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnFunc AllowedAttributesOnFunc @endlink</code></td>
<td>Invalid attribute found on the FunctionDefinition object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidUnitDefId InvalidUnitDefId @endlink</code></td>
<td>Invalid 'id' attribute value for a UnitDefinition object</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidSubstanceRedefinition InvalidSubstanceRedefinition @endlink</code></td>
<td>Invalid redefinition of built-in type 'substance'</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidLengthRedefinition InvalidLengthRedefinition @endlink</code></td>
<td>Invalid redefinition of built-in type 'length'</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidAreaRedefinition InvalidAreaRedefinition @endlink</code></td>
<td>Invalid redefinition of built-in type name 'area'</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidTimeRedefinition InvalidTimeRedefinition @endlink</code></td>
<td>Invalid redefinition of built-in type name 'time'</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidVolumeRedefinition InvalidVolumeRedefinition @endlink</code></td>
<td>Invalid redefinition of built-in type name 'volume'</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#VolumeLitreDefExponentNotOne VolumeLitreDefExponentNotOne @endlink</code></td>
<td>Must use 'exponent'=1 when defining 'volume' in terms of litres</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#VolumeMetreDefExponentNot3 VolumeMetreDefExponentNot3 @endlink</code></td>
<td>Must use 'exponent'=3 when defining 'volume' in terms of metres</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#EmptyListOfUnits EmptyListOfUnits @endlink</code></td>
<td>An empty list of Unit objects is not permitted in a UnitDefinition object</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidUnitKind InvalidUnitKind @endlink</code></td>
<td>Invalid value for the 'kind' attribute of a UnitDefinition object</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OffsetNoLongerValid OffsetNoLongerValid @endlink</code></td>
<td>Unit attribute 'offset' is not supported in this Level+Version of SBML</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#CelsiusNoLongerValid CelsiusNoLongerValid @endlink</code></td>
<td>Unit name 'Celsius' is not defined in this Level+Version of SBML</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#EmptyUnitListElement EmptyUnitListElement @endlink</code></td>
<td>A ListOfUnits object must not be empty</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneListOfUnitsPerUnitDef OneListOfUnitsPerUnitDef @endlink</code></td>
<td>At most one ListOfUnits object is allowed inside a UnitDefinition object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyUnitsInListOfUnits OnlyUnitsInListOfUnits @endlink</code></td>
<td>Only Unit, Notes and Annotation objects are allowed in ListOfUnits objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnUnitDefinition AllowedAttributesOnUnitDefinition @endlink</code></td>
<td>Invalid attribute found on the UnitDefinition object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfUnits AllowedAttributesOnListOfUnits @endlink</code></td>
<td>Invalid attribute found on the ListOfUnits object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnUnit AllowedAttributesOnUnit @endlink</code></td>
<td>Invalid attribute found on the Unit object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ZeroDimensionalCompartmentSize ZeroDimensionalCompartmentSize @endlink</code></td>
<td>Invalid use of the 'size' attribute for a zero-dimensional compartment</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ZeroDimensionalCompartmentUnits ZeroDimensionalCompartmentUnits @endlink</code></td>
<td>Invalid use of the 'units' attribute for a zero-dimensional compartment</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ZeroDimensionalCompartmentConst ZeroDimensionalCompartmentConst @endlink</code></td>
<td>Zero-dimensional compartments must be defined to be constant</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#UndefinedOutsideCompartment UndefinedOutsideCompartment @endlink</code></td>
<td>Invalid value for the 'outside' attribute of a Compartment object</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#RecursiveCompartmentContainment RecursiveCompartmentContainment @endlink</code></td>
<td>Recursive nesting of compartments via the 'outside' attribute is not permitted</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ZeroDCompartmentContainment ZeroDCompartmentContainment @endlink</code></td>
<td>Invalid nesting of zero-dimensional compartments</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#Invalid1DCompartmentUnits Invalid1DCompartmentUnits @endlink</code></td>
<td>Invalid value for the 'units' attribute of a one-dimensional compartment</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#Invalid2DCompartmentUnits Invalid2DCompartmentUnits @endlink</code></td>
<td>Invalid value for the 'units' attribute of a two-dimensional compartment</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#Invalid3DCompartmentUnits Invalid3DCompartmentUnits @endlink</code></td>
<td>Invalid value for the 'units' attribute of a three-dimensional compartment</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidCompartmentTypeRef InvalidCompartmentTypeRef @endlink</code></td>
<td>Invalid value for the 'compartmentType' attribute of a compartment</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneDimensionalCompartmentUnits OneDimensionalCompartmentUnits @endlink</code></td>
<td>No units defined for 1-D compartment</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#TwoDimensionalCompartmentUnits TwoDimensionalCompartmentUnits @endlink</code></td>
<td>No units defined for 2-D compartment</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ThreeDimensionalCompartmentUnits ThreeDimensionalCompartmentUnits @endlink</code></td>
<td>No units defined for 3-D Compartment object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnCompartment AllowedAttributesOnCompartment @endlink</code></td>
<td>Invalid attribute found on Compartment object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoUnitsOnCompartment NoUnitsOnCompartment @endlink</code></td>
<td>No units defined for Compartment object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidSpeciesCompartmentRef InvalidSpeciesCompartmentRef @endlink</code></td>
<td>Invalid value found for Species 'compartment' attribute</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#HasOnlySubsNoSpatialUnits HasOnlySubsNoSpatialUnits @endlink</code></td>
<td>Attribute 'spatialSizeUnits' must not be set if 'hasOnlySubstanceUnits'='true'</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoSpatialUnitsInZeroD NoSpatialUnitsInZeroD @endlink</code></td>
<td>Attribute 'spatialSizeUnits' must not be set if the compartment is zero-dimensional</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoConcentrationInZeroD NoConcentrationInZeroD @endlink</code></td>
<td>Attribute 'initialConcentration' must not be set if the compartment is zero-dimensional</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SpatialUnitsInOneD SpatialUnitsInOneD @endlink</code></td>
<td>Invalid value for 'spatialSizeUnits' attribute of a one-dimensional compartment</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SpatialUnitsInTwoD SpatialUnitsInTwoD @endlink</code></td>
<td>Invalid value for the 'spatialSizeUnits' attribute of a two-dimensional compartment</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SpatialUnitsInThreeD SpatialUnitsInThreeD @endlink</code></td>
<td>Invalid value for the 'spatialSizeUnits' attribute of a three-dimensional compartment</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidSpeciesSusbstanceUnits InvalidSpeciesSusbstanceUnits @endlink</code></td>
<td>Invalid value for a Species 'units' attribute</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#BothAmountAndConcentrationSet BothAmountAndConcentrationSet @endlink</code></td>
<td>Cannot set both 'initialConcentration' and 'initialAmount' attributes simultaneously</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NonBoundarySpeciesAssignedAndUsed NonBoundarySpeciesAssignedAndUsed @endlink</code></td>
<td>Cannot use a non-boundary species in both reactions and rules simultaneously</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NonConstantSpeciesUsed NonConstantSpeciesUsed @endlink</code></td>
<td>Cannot use a constant, non-boundary species as a reactant or product</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidSpeciesTypeRef InvalidSpeciesTypeRef @endlink</code></td>
<td>Invalid value for the 'speciesType' attribute of a species</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MultSpeciesSameTypeInCompartment MultSpeciesSameTypeInCompartment @endlink</code></td>
<td>Cannot have multiple species of the same species type in the same compartment</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MissingSpeciesCompartment MissingSpeciesCompartment @endlink</code></td>
<td>Missing value for the 'compartment' attribute</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SpatialSizeUnitsRemoved SpatialSizeUnitsRemoved @endlink</code></td>
<td>Attribute 'spatialSizeUnits' is not supported in this Level+Version of SBML</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SubstanceUnitsOnSpecies SubstanceUnitsOnSpecies @endlink</code></td>
<td>No substance units defined for the species</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ConversionFactorOnSpecies ConversionFactorOnSpecies @endlink</code></td>
<td>Invalid value for the 'conversionFactor' attribute</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnSpecies AllowedAttributesOnSpecies @endlink</code></td>
<td>Invalid attribute found on Species object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidParameterUnits InvalidParameterUnits @endlink</code></td>
<td>Invalid value for the 'units' attribute of a Parameter object</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ParameterUnits ParameterUnits @endlink</code></td>
<td>No units defined for the parameter</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ConversionFactorMustConstant ConversionFactorMustConstant @endlink</code></td>
<td>A conversion factor must reference a Parameter object declared to be a constant</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnParameter AllowedAttributesOnParameter @endlink</code></td>
<td>Invalid attribute found on Parameter object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidInitAssignSymbol InvalidInitAssignSymbol @endlink</code></td>
<td>Invalid value for the 'symbol' attribute of an InitialAssignment object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MultipleInitAssignments MultipleInitAssignments @endlink</code></td>
<td>Multiple initial assignments for the same 'symbol' value are not allowed</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InitAssignmentAndRuleForSameId InitAssignmentAndRuleForSameId @endlink</code></td>
<td>Cannot set a value using both an initial assignment and an assignment rule simultaneously</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneMathElementPerInitialAssign OneMathElementPerInitialAssign @endlink</code></td>
<td>An InitialAssignment object must contain one <code>&lt;math&gt;</code> element</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnInitialAssign AllowedAttributesOnInitialAssign @endlink</code></td>
<td>Invalid attribute found on an InitialAssignment object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidAssignRuleVariable InvalidAssignRuleVariable @endlink</code></td>
<td>Invalid value for the 'variable' attribute of an AssignmentRule object</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidRateRuleVariable InvalidRateRuleVariable @endlink</code></td>
<td>Invalid value for the 'variable' attribute of a RateRule object</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AssignmentToConstantEntity AssignmentToConstantEntity @endlink</code></td>
<td>An assignment rule cannot assign an entity declared to be constant</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#RateRuleForConstantEntity RateRuleForConstantEntity @endlink</code></td>
<td>A rate rule cannot assign an entity declared to be constant</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#CircularRuleDependency CircularRuleDependency @endlink</code></td>
<td>Circular dependencies involving rules and reactions are not permitted</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneMathElementPerRule OneMathElementPerRule @endlink</code></td>
<td>A rule object must contain one <code>&lt;math&gt;</code> element</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnAssignRule AllowedAttributesOnAssignRule @endlink</code></td>
<td>Invalid attribute found on an AssignmentRule object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnRateRule AllowedAttributesOnRateRule @endlink</code></td>
<td>Invalid attribute found on a RateRule object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnAlgRule AllowedAttributesOnAlgRule @endlink</code></td>
<td>Invalid attribute found on an AlgebraicRule object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ConstraintMathNotBoolean ConstraintMathNotBoolean @endlink</code></td>
<td>A Constraint object's <code>&lt;math&gt;</code> must evaluate to a Boolean value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#IncorrectOrderInConstraint IncorrectOrderInConstraint @endlink</code></td>
<td>Subobjects inside the Constraint object are not in the prescribed order</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ConstraintNotInXHTMLNamespace ConstraintNotInXHTMLNamespace @endlink</code></td>
<td>A Constraint's Message subobject must be in the XHTML XML namespace</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ConstraintContainsXMLDecl ConstraintContainsXMLDecl @endlink</code></td>
<td>XML declarations are not permitted within Constraint's Message objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ConstraintContainsDOCTYPE ConstraintContainsDOCTYPE @endlink</code></td>
<td>XML DOCTYPE elements are not permitted within Constraint's Message objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidConstraintContent InvalidConstraintContent @endlink</code></td>
<td>Invalid content for a Constraint object's Message object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneMathElementPerConstraint OneMathElementPerConstraint @endlink</code></td>
<td>A Constraint object must contain one <code>&lt;math&gt;</code> element</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneMessageElementPerConstraint OneMessageElementPerConstraint @endlink</code></td>
<td>A Constraint object must contain one Message subobject</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnConstraint AllowedAttributesOnConstraint @endlink</code></td>
<td>Invalid attribute found on Constraint object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoReactantsOrProducts NoReactantsOrProducts @endlink</code></td>
<td>Cannot have a reaction with neither reactants nor products</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#IncorrectOrderInReaction IncorrectOrderInReaction @endlink</code></td>
<td>Subobjects inside the Reaction object are not in the prescribed order</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#EmptyListInReaction EmptyListInReaction @endlink</code></td>
<td>Reaction components, if present, cannot be empty</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidReactantsProductsList InvalidReactantsProductsList @endlink</code></td>
<td>Invalid object found in the list of reactants or products</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidModifiersList InvalidModifiersList @endlink</code></td>
<td>Invalid object found in the list of modifiers</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneSubElementPerReaction OneSubElementPerReaction @endlink</code></td>
<td>A Reaction object can only contain one of each allowed type of object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#CompartmentOnReaction CompartmentOnReaction @endlink</code></td>
<td>Invalid value for the Reaction 'compartment' attribute</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnReaction AllowedAttributesOnReaction @endlink</code></td>
<td>Invalid attribute for a Reaction object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidSpeciesReference InvalidSpeciesReference @endlink</code></td>
<td>Invalid 'species' attribute value in SpeciesReference object</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#BothStoichiometryAndMath BothStoichiometryAndMath @endlink</code></td>
<td>The 'stoichiometry' attribute and StoichiometryMath subobject are mutually exclusive</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnSpeciesReference AllowedAttributesOnSpeciesReference @endlink</code></td>
<td>Invalid attribute found on the SpeciesReference object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnModifier AllowedAttributesOnModifier @endlink</code></td>
<td>Invalid attribute found on the ModifierSpeciesReference object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#UndeclaredSpeciesRef UndeclaredSpeciesRef @endlink</code></td>
<td>Unknown species referenced in the kinetic law <code>&lt;math&gt;</code> formula</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#IncorrectOrderInKineticLaw IncorrectOrderInKineticLaw @endlink</code></td>
<td>Incorrect ordering of components in the KineticLaw object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#EmptyListInKineticLaw EmptyListInKineticLaw @endlink</code></td>
<td>The list of parameters, if present, cannot be empty</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NonConstantLocalParameter NonConstantLocalParameter @endlink</code></td>
<td>Parameters local to a KineticLaw object must have a 'constant' attribute value of 'true'</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SubsUnitsNoLongerValid SubsUnitsNoLongerValid @endlink</code></td>
<td>Attribute 'substanceUnits' is not supported in this Level+Version of SBML</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#TimeUnitsNoLongerValid TimeUnitsNoLongerValid @endlink</code></td>
<td>Attribute 'timeUnits' is not supported in this Level+Version of SBML</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneListOfPerKineticLaw OneListOfPerKineticLaw @endlink</code></td>
<td>Only one ListOfLocalParameters object is permitted within a KineticLaw object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyLocalParamsInListOfLocalParams OnlyLocalParamsInListOfLocalParams @endlink</code></td>
<td>Only LocalParameter, Notes and Annotation objects are allowed in ListOfLocalParameter objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfLocalParam AllowedAttributesOnListOfLocalParam @endlink</code></td>
<td>Invalid attribute found on the ListOfLocalParameters object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneMathPerKineticLaw OneMathPerKineticLaw @endlink</code></td>
<td>Only one <code>&lt;math&gt;</code> element is allowed in a KineticLaw object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#UndeclaredSpeciesInStoichMath UndeclaredSpeciesInStoichMath @endlink</code></td>
<td>Unknown species referenced in the StoichiometryMath object's <code>&lt;math&gt;</code> formula</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnKineticLaw AllowedAttributesOnKineticLaw @endlink</code></td>
<td>Invalid attribute found on the KineticLaw object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfSpeciesRef AllowedAttributesOnListOfSpeciesRef @endlink</code></td>
<td>Invalid attribute found on the ListOfSpeciesReferences object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfMods AllowedAttributesOnListOfMods @endlink</code></td>
<td>Invalid attribute found on the ListOfModifiers object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnLocalParameter AllowedAttributesOnLocalParameter @endlink</code></td>
<td>Invalid attribute found on the LocalParameter object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MissingTriggerInEvent MissingTriggerInEvent @endlink</code></td>
<td>The Event object is missing a Trigger subobject</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#TriggerMathNotBoolean TriggerMathNotBoolean @endlink</code></td>
<td>A Trigger object's <code>&lt;math&gt;</code> expression must evaluate to a Boolean value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MissingEventAssignment MissingEventAssignment @endlink</code></td>
<td>The Event object is missing an EventAssignment subobject</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#TimeUnitsEvent TimeUnitsEvent @endlink</code></td>
<td>Units referenced by 'timeUnits' attribute are not compatible with units of time</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#IncorrectOrderInEvent IncorrectOrderInEvent @endlink</code></td>
<td>Incorrect ordering of components in Event object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ValuesFromTriggerTimeNeedDelay ValuesFromTriggerTimeNeedDelay @endlink</code></td>
<td>Attribute 'useValuesFromTriggerTime'='false', but the Event object does not define a delay</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DelayNeedsValuesFromTriggerTime DelayNeedsValuesFromTriggerTime @endlink</code></td>
<td>The use of a Delay object requires the Event attribute 'useValuesFromTriggerTime'</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneMathPerTrigger OneMathPerTrigger @endlink</code></td>
<td>A Trigger object must have one <code>&lt;math&gt;</code> element</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneMathPerDelay OneMathPerDelay @endlink</code></td>
<td>A Delay object must have one <code>&lt;math&gt;</code> element</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidEventAssignmentVariable InvalidEventAssignmentVariable @endlink</code></td>
<td>Invalid 'variable' attribute value in Event object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#EventAssignmentForConstantEntity EventAssignmentForConstantEntity @endlink</code></td>
<td>An EventAssignment object cannot assign to a component having attribute 'constant'='true'</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneMathPerEventAssignment OneMathPerEventAssignment @endlink</code></td>
<td>An EventAssignment object must have one <code>&lt;math&gt;</code> element</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnEventAssignment AllowedAttributesOnEventAssignment @endlink</code></td>
<td>Invalid attribute found on the EventAssignment object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyOneDelayPerEvent OnlyOneDelayPerEvent @endlink</code></td>
<td>An Event object can only have one Delay subobject</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneListOfEventAssignmentsPerEvent OneListOfEventAssignmentsPerEvent @endlink</code></td>
<td>An Event object can only have one ListOfEventAssignments subobject</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyEventAssignInListOfEventAssign OnlyEventAssignInListOfEventAssign @endlink</code></td>
<td>Only EventAssignment, Notes and Annotation objects are allowed in ListOfEventAssignments</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnListOfEventAssign AllowedAttributesOnListOfEventAssign @endlink</code></td>
<td>Invalid attribute found on the ListOfEventAssignments object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnEvent AllowedAttributesOnEvent @endlink</code></td>
<td>Invalid attribute found on the Event object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnTrigger AllowedAttributesOnTrigger @endlink</code></td>
<td>Invalid attribute found on the Trigger object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnDelay AllowedAttributesOnDelay @endlink</code></td>
<td>Invalid attribute found on the Delay object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#PersistentNotBoolean PersistentNotBoolean @endlink</code></td>
<td>The Trigger attribute 'persistent' must evaluate to a Boolean value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InitialValueNotBoolean InitialValueNotBoolean @endlink</code></td>
<td>The Trigger attribute 'initialValue' must evaluate to a Boolean value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OnlyOnePriorityPerEvent OnlyOnePriorityPerEvent @endlink</code></td>
<td>An Event object can only have one Priority subobject</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OneMathPerPriority OneMathPerPriority @endlink</code></td>
<td>A Priority object must have one <code>&lt;math&gt;</code> element</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AllowedAttributesOnPriority AllowedAttributesOnPriority @endlink</code></td>
<td>Invalid attribute found on the Priority object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#GeneralWarningNotSpecified GeneralWarningNotSpecified @endlink</code></td>
<td>Unknown error</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#CompartmentShouldHaveSize CompartmentShouldHaveSize @endlink</code></td>
<td>It's best to define a size for every compartment in a model</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SpeciesShouldHaveValue SpeciesShouldHaveValue @endlink</code></td>
<td>It's best to define an initial amount or initial concentration for every species in a model</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ParameterShouldHaveUnits ParameterShouldHaveUnits @endlink</code></td>
<td>It's best to declare units for every parameter in a model</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#LocalParameterShadowsId LocalParameterShadowsId @endlink</code></td>
<td>Local parameters defined within a kinetic law shadow global object symbols</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#CannotConvertToL1V1 CannotConvertToL1V1 @endlink</code></td>
<td>Cannot convert to SBML Level 1 Version 1</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoEventsInL1 NoEventsInL1 @endlink</code></td>
<td>SBML Level 1 does not support events</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoFunctionDefinitionsInL1 NoFunctionDefinitionsInL1 @endlink</code></td>
<td>SBML Level 1 does not support function definitions</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoConstraintsInL1 NoConstraintsInL1 @endlink</code></td>
<td>SBML Level 1 does not support constraints</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoInitialAssignmentsInL1 NoInitialAssignmentsInL1 @endlink</code></td>
<td>SBML Level 1 does not support initial assignments</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoSpeciesTypesInL1 NoSpeciesTypesInL1 @endlink</code></td>
<td>SBML Level 1 does not support species types</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoCompartmentTypeInL1 NoCompartmentTypeInL1 @endlink</code></td>
<td>SBML Level 1 does not support compartment types</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoNon3DCompartmentsInL1 NoNon3DCompartmentsInL1 @endlink</code></td>
<td>SBML Level 1 only supports three-dimensional compartments</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoFancyStoichiometryMathInL1 NoFancyStoichiometryMathInL1 @endlink</code></td>
<td>SBML Level 1 does not support non-integer nor non-rational stoichiometry formulas</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoNonIntegerStoichiometryInL1 NoNonIntegerStoichiometryInL1 @endlink</code></td>
<td>SBML Level 1 does not support non-integer 'stoichiometry' attribute values</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoUnitMultipliersOrOffsetsInL1 NoUnitMultipliersOrOffsetsInL1 @endlink</code></td>
<td>SBML Level 1 does not support multipliers or offsets in unit definitions</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SpeciesCompartmentRequiredInL1 SpeciesCompartmentRequiredInL1 @endlink</code></td>
<td>In SBML Level 1, a value for 'compartment' is mandatory in species definitions</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoSpeciesSpatialSizeUnitsInL1 NoSpeciesSpatialSizeUnitsInL1 @endlink</code></td>
<td>SBML Level 1 does not support species 'spatialSizeUnits' settings</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoSBOTermsInL1 NoSBOTermsInL1 @endlink</code></td>
<td>SBML Level 1 does not support the 'sboTerm' attribute</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#StrictUnitsRequiredInL1 StrictUnitsRequiredInL1 @endlink</code></td>
<td>SBML Level 1 requires strict unit consistency</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ConversionFactorNotInL1 ConversionFactorNotInL1 @endlink</code></td>
<td>SBML Level 1 does not support the 'conversionFactor' attribute</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#CompartmentNotOnL1Reaction CompartmentNotOnL1Reaction @endlink</code></td>
<td>SBML Level 1 does not support the 'compartment' attribute on Reaction objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ExtentUnitsNotSubstance ExtentUnitsNotSubstance @endlink</code></td>
<td>Units of extent must be compatible with units of substance</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#GlobalUnitsNotDeclared GlobalUnitsNotDeclared @endlink</code></td>
<td>Global units must be refer to unit kind or unitDefinition.</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#HasOnlySubstanceUnitsNotinL1 HasOnlySubstanceUnitsNotinL1 @endlink</code></td>
<td>The concept of hasOnlySubstanceUnits was not available in SBML Level 1.</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AvogadroNotSupported AvogadroNotSupported @endlink</code></td>
<td>Avogadro not supported in Levels 2 and 1.</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoConstraintsInL2v1 NoConstraintsInL2v1 @endlink</code></td>
<td>SBML Level 2 Version 1 does not support Constraint objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoInitialAssignmentsInL2v1 NoInitialAssignmentsInL2v1 @endlink</code></td>
<td>SBML Level 2 Version 1 does not support InitialAssignment objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoSpeciesTypeInL2v1 NoSpeciesTypeInL2v1 @endlink</code></td>
<td>SBML Level 2 Version 1 does not support SpeciesType objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoCompartmentTypeInL2v1 NoCompartmentTypeInL2v1 @endlink</code></td>
<td>SBML Level 2 Version 1 does not support CompartmentType objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoSBOTermsInL2v1 NoSBOTermsInL2v1 @endlink</code></td>
<td>SBML Level 2 Version 1 does not support the 'sboTerm' attribute</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoIdOnSpeciesReferenceInL2v1 NoIdOnSpeciesReferenceInL2v1 @endlink</code></td>
<td>SBML Level 2 Version 1 does not support the 'id' attribute on SpeciesReference objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoDelayedEventAssignmentInL2v1 NoDelayedEventAssignmentInL2v1 @endlink</code></td>
<td>SBML Level 2 Version 1 does not support the 'useValuesFromTriggerTime' attribute</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#StrictUnitsRequiredInL2v1 StrictUnitsRequiredInL2v1 @endlink</code></td>
<td>SBML Level 2 Version 1 requires strict unit consistency</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#IntegerSpatialDimensions IntegerSpatialDimensions @endlink</code></td>
<td>SBML Level 2 Version 1 requires that compartments have spatial dimensions of 0-3</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#StoichiometryMathNotYetSupported StoichiometryMathNotYetSupported @endlink</code></td>
<td>Conversion to StoichiometryMath objects not yet supported</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#PriorityLostFromL3 PriorityLostFromL3 @endlink</code></td>
<td>SBML Level 2 Version 1 does not support priorities on Event objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NonPersistentNotSupported NonPersistentNotSupported @endlink</code></td>
<td>SBML Level 2 Version 1 does not support the 'persistent' attribute on Trigger objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InitialValueFalseEventNotSupported InitialValueFalseEventNotSupported @endlink</code></td>
<td>SBML Level 2 Version 1 does not support the 'initialValue' attribute on Trigger objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SBOTermNotUniversalInL2v2 SBOTermNotUniversalInL2v2 @endlink</code></td>
<td>The 'sboTerm' attribute is invalid for this component in SBML Level 2 Version 2</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoUnitOffsetInL2v2 NoUnitOffsetInL2v2 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'offset' attribute on Unit objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoKineticLawTimeUnitsInL2v2 NoKineticLawTimeUnitsInL2v2 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'timeUnits' attribute on KineticLaw objects</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoKineticLawSubstanceUnitsInL2v2 NoKineticLawSubstanceUnitsInL2v2 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'substanceUnits' attribute on KineticLaw objects</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoDelayedEventAssignmentInL2v2 NoDelayedEventAssignmentInL2v2 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'useValuesFromTriggerTime' attribute</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ModelSBOBranchChangedBeyondL2v2 ModelSBOBranchChangedBeyondL2v2 @endlink</code></td>
<td>The allowable 'sboTerm' attribute values for Model objects differ for this SBML Level+Version</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#StrictUnitsRequiredInL2v2 StrictUnitsRequiredInL2v2 @endlink</code></td>
<td>SBML Level 2 Version 2 requires strict unit consistency</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#StrictSBORequiredInL2v2 StrictSBORequiredInL2v2 @endlink</code></td>
<td>SBML Level 2 Version 2 requires strict SBO term consistency</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DuplicateAnnotationInvalidInL2v2 DuplicateAnnotationInvalidInL2v2 @endlink</code></td>
<td>Duplicate top-level annotations are invalid in SBML Level 2 Version 2</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoUnitOffsetInL2v3 NoUnitOffsetInL2v3 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'offset' attribute on Unit objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoKineticLawTimeUnitsInL2v3 NoKineticLawTimeUnitsInL2v3 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'timeUnits' attribute on KineticLaw objects</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoKineticLawSubstanceUnitsInL2v3 NoKineticLawSubstanceUnitsInL2v3 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'substanceUnits' attribute on KineticLaw objects</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoSpeciesSpatialSizeUnitsInL2v3 NoSpeciesSpatialSizeUnitsInL2v3 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'spatialSizeUnit' attribute on Species objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoEventTimeUnitsInL2v3 NoEventTimeUnitsInL2v3 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'timeUnits' attribute on Event objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoDelayedEventAssignmentInL2v3 NoDelayedEventAssignmentInL2v3 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'useValuesFromTriggerTime' attribute</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ModelSBOBranchChangedBeyondL2v3 ModelSBOBranchChangedBeyondL2v3 @endlink</code></td>
<td>The allowable 'sboTerm' attribute values for Model objects differ for this SBML Level+Version</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#StrictUnitsRequiredInL2v3 StrictUnitsRequiredInL2v3 @endlink</code></td>
<td>SBML Level 2 Version 3 requires strict unit consistency</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#StrictSBORequiredInL2v3 StrictSBORequiredInL2v3 @endlink</code></td>
<td>SBML Level 2 Version 3 requires strict SBO term consistency</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DuplicateAnnotationInvalidInL2v3 DuplicateAnnotationInvalidInL2v3 @endlink</code></td>
<td>Duplicate top-level annotations are invalid in SBML Level 2 Version 3</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoUnitOffsetInL2v4 NoUnitOffsetInL2v4 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'offset' attribute on Unit objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoKineticLawTimeUnitsInL2v4 NoKineticLawTimeUnitsInL2v4 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'timeUnits' attribute on KineticLaw objects</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoKineticLawSubstanceUnitsInL2v4 NoKineticLawSubstanceUnitsInL2v4 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'substanceUnits' attribute on KineticLaw objects</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoSpeciesSpatialSizeUnitsInL2v4 NoSpeciesSpatialSizeUnitsInL2v4 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'spatialSizeUnit' attribute on Species objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoEventTimeUnitsInL2v4 NoEventTimeUnitsInL2v4 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'timeUnits' attribute on Event objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ModelSBOBranchChangedInL2v4 ModelSBOBranchChangedInL2v4 @endlink</code></td>
<td>The allowable 'sboTerm' attribute values for Model objects differ for this SBML Level+Version</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DuplicateAnnotationInvalidInL2v4 DuplicateAnnotationInvalidInL2v4 @endlink</code></td>
<td>Duplicate top-level annotations are invalid in SBML Level 2 Version 4</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoSpeciesTypeInL3v1 NoSpeciesTypeInL3v1 @endlink</code></td>
<td>SBML Level 3 Version 1 does not support SpeciesType objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoCompartmentTypeInL3v1 NoCompartmentTypeInL3v1 @endlink</code></td>
<td>SBML Level 3 Version 1 does not support CompartmentType objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoUnitOffsetInL3v1 NoUnitOffsetInL3v1 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'offset' attribute on Unit objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoKineticLawTimeUnitsInL3v1 NoKineticLawTimeUnitsInL3v1 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'timeUnits' attribute on KineticLaw objects</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoKineticLawSubstanceUnitsInL3v1 NoKineticLawSubstanceUnitsInL3v1 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'substanceUnits' attribute on KineticLaw objects</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoSpeciesSpatialSizeUnitsInL3v1 NoSpeciesSpatialSizeUnitsInL3v1 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'spatialSizeUnit' attribute on Species objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoEventTimeUnitsInL3v1 NoEventTimeUnitsInL3v1 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'timeUnits' attribute on Event objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ModelSBOBranchChangedInL3v1 ModelSBOBranchChangedInL3v1 @endlink</code></td>
<td>The allowable 'sboTerm' attribute values for Model objects differ for this SBML Level+Version</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DuplicateAnnotationInvalidInL3v1 DuplicateAnnotationInvalidInL3v1 @endlink</code></td>
<td>Duplicate top-level annotations are invalid in SBML Level 3 Version 1</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoCompartmentOutsideInL3v1 NoCompartmentOutsideInL3v1 @endlink</code></td>
<td>This Level+Version of SBML does not support the 'outside' attribute on Compartment objects</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoStoichiometryMathInL3v1 NoStoichiometryMathInL3v1 @endlink</code></td>
<td>This Level+Version of SBML does not support the StoichiometryMath object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidSBMLLevelVersion InvalidSBMLLevelVersion @endlink</code></td>
<td>Unknown Level+Version combination of SBML</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AnnotationNotesNotAllowedLevel1 AnnotationNotesNotAllowedLevel1 @endlink</code></td>
<td>Annotation objects on the SBML container element are not permitted in SBML Level 1</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidRuleOrdering InvalidRuleOrdering @endlink</code></td>
<td>Invalid ordering of rules</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#RequiredPackagePresent RequiredPackagePresent @endlink</code></td>
<td>The SBML document requires an SBML Level 3 package unavailable in this software</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#UnrequiredPackagePresent UnrequiredPackagePresent @endlink</code></td>
<td>The SBML document uses an SBML Level 3 package unavailable in this software</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#PackageRequiredShouldBeFalse PackageRequiredShouldBeFalse @endlink</code></td>
<td>This package expects required to be false.</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SubsUnitsAllowedInKL SubsUnitsAllowedInKL @endlink</code></td>
<td>Disallowed value for attribute 'substanceUnits' on KineticLaw object</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#TimeUnitsAllowedInKL TimeUnitsAllowedInKL @endlink</code></td>
<td>Disallowed value for attribute 'timeUnits' on KineticLaw object</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#FormulaInLevel1KL FormulaInLevel1KL @endlink</code></td>
<td>Only predefined functions are allowed in SBML Level 1 formulas</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#L3SubstanceUnitsOnModel L3SubstanceUnitsOnModel @endlink</code></td>
<td>Invalid 'substanceUnits' attribute value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#TimeUnitsRemoved TimeUnitsRemoved @endlink</code></td>
<td>This Level+Version of SBML does not support the 'timeUnits' attribute on Event objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#BadMathML BadMathML @endlink</code></td>
<td>Invalid MathML expression</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#FailedMathMLReadOfDouble FailedMathMLReadOfDouble @endlink</code></td>
<td>Missing or invalid floating-point number in MathML expression</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#FailedMathMLReadOfInteger FailedMathMLReadOfInteger @endlink</code></td>
<td>Missing or invalid integer in MathML expression</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#FailedMathMLReadOfExponential FailedMathMLReadOfExponential @endlink</code></td>
<td>Missing or invalid exponential expression in MathML</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#FailedMathMLReadOfRational FailedMathMLReadOfRational @endlink</code></td>
<td>Missing or invalid rational expression in MathML</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#BadMathMLNodeType BadMathMLNodeType @endlink</code></td>
<td>Invalid MathML element</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoTimeSymbolInFunctionDef NoTimeSymbolInFunctionDef @endlink</code></td>
<td>Use of <code>&lt;csymbol&gt;</code> for 'time' not allowed within FunctionDefinition objects</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NoBodyInFunctionDef NoBodyInFunctionDef @endlink</code></td>
<td>There must be a <code>&lt;lambda&gt;</code> body within the <code>&lt;math&gt;</code> element of a FunctionDefinition object</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#DanglingUnitSIdRef DanglingUnitSIdRef @endlink</code></td>
<td>Units must refer to valid unit or unitDefinition</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#RDFMissingAboutTag RDFMissingAboutTag @endlink</code></td>
<td>RDF missing the <code>&lt;about&gt;</code> tag.</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#RDFEmptyAboutTag RDFEmptyAboutTag @endlink</code></td>
<td>RDF empty <code>&lt;about&gt;</code> tag.</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#RDFAboutTagNotMetaid RDFAboutTagNotMetaid @endlink</code></td>
<td>RDF <code>&lt;about&gt;</code> tag is not metaid.</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#RDFNotCompleteModelHistory RDFNotCompleteModelHistory @endlink</code></td>
<td>RDF does not contain valid ModelHistory.</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#RDFNotModelHistory RDFNotModelHistory @endlink</code></td>
<td>RDF does not result in a ModelHistory.</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#AnnotationNotElement AnnotationNotElement @endlink</code></td>
<td>Annotation must contain element.</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#UndeclaredUnits UndeclaredUnits @endlink</code></td>
<td>Missing unit declarations on parameters or literal numbers in expression</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#UndeclaredTimeUnitsL3 UndeclaredTimeUnitsL3 @endlink</code></td>
<td>Unable to verify consistency of units: the unit of time has not been declared</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#UndeclaredExtentUnitsL3 UndeclaredExtentUnitsL3 @endlink</code></td>
<td>Unable to verify consistency of units: the units of reaction extent have not been declared</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#UndeclaredObjectUnitsL3 UndeclaredObjectUnitsL3 @endlink</code></td>
<td>Unable to verify consistency of units: encountered a model entity with no declared units</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#UnrecognisedSBOTerm UnrecognisedSBOTerm @endlink</code></td>
<td>Unrecognized 'sboTerm' attribute value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ObseleteSBOTerm ObseleteSBOTerm @endlink</code></td>
<td>Obsolete 'sboTerm' attribute value</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#IncorrectCompartmentSpatialDimensions IncorrectCompartmentSpatialDimensions @endlink</code></td>
<td>In SBML Level 1, only three-dimensional compartments are allowed</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#CompartmentTypeNotValidAttribute CompartmentTypeNotValidAttribute @endlink</code></td>
<td>CompartmentType objects are not available in this Level+Version of SBML</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ConstantNotValidAttribute ConstantNotValidAttribute @endlink</code></td>
<td>This Level+Version of SBML does not support the 'constant' attribute on this component</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MetaIdNotValidAttribute MetaIdNotValidAttribute @endlink</code></td>
<td>Attribute 'metaid' is not available in SBML Level 1</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SBOTermNotValidAttributeBeforeL2V3 SBOTermNotValidAttributeBeforeL2V3 @endlink</code></td>
<td>The 'sboTerm' attribute is not available on this component before SBML Level 2 Version 3</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidL1CompartmentUnits InvalidL1CompartmentUnits @endlink</code></td>
<td>Invalid units for a compartment in SBML Level 1</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#L1V1CompartmentVolumeReqd L1V1CompartmentVolumeReqd @endlink</code></td>
<td>In SBML Level 1, a compartment's volume must be specified</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#CompartmentTypeNotValidComponent CompartmentTypeNotValidComponent @endlink</code></td>
<td>CompartmentType objects are not available in this Level+Version of SBML</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ConstraintNotValidComponent ConstraintNotValidComponent @endlink</code></td>
<td>Constraint objects are not available in this Level+Version of SBML</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#EventNotValidComponent EventNotValidComponent @endlink</code></td>
<td>Event objects are not available in this Level+Version of SBML</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SBOTermNotValidAttributeBeforeL2V2 SBOTermNotValidAttributeBeforeL2V2 @endlink</code></td>
<td>The 'sboTerm' attribute is invalid for this component before Level 2 Version 2</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#FuncDefNotValidComponent FuncDefNotValidComponent @endlink</code></td>
<td>FunctionDefinition objects are not available in this Level+Version of SBML</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InitialAssignNotValidComponent InitialAssignNotValidComponent @endlink</code></td>
<td>InitialAssignment objects are not available in this Level+Version of SBML</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#VariableNotValidAttribute VariableNotValidAttribute @endlink</code></td>
<td>Attribute 'variable' is not available on this component in this Level+Version of SBML</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#UnitsNotValidAttribute UnitsNotValidAttribute @endlink</code></td>
<td>Attribute 'units' is not available on this component in this Level+Version of SBML</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#ConstantSpeciesNotValidAttribute ConstantSpeciesNotValidAttribute @endlink</code></td>
<td>Attribute 'constant' is not available on Species objects in SBML Level 1</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SpatialSizeUnitsNotValidAttribute SpatialSizeUnitsNotValidAttribute @endlink</code></td>
<td>Attribute 'spatialSizeUnits' is not available on Species objects in SBML Level 1</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SpeciesTypeNotValidAttribute SpeciesTypeNotValidAttribute @endlink</code></td>
<td>Attribute 'speciesType' is not available on Species objects in SBML Level 1</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#HasOnlySubsUnitsNotValidAttribute HasOnlySubsUnitsNotValidAttribute @endlink</code></td>
<td>Attribute 'hasOnlySubstanceUnits' is not available on Species objects in SBML Level 1</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#IdNotValidAttribute IdNotValidAttribute @endlink</code></td>
<td>Attribute 'id' is not available on SpeciesReference objects in SBML Level 1</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#NameNotValidAttribute NameNotValidAttribute @endlink</code></td>
<td>Attribute 'name' is not available on SpeciesReference objects in SBML Level 1</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#SpeciesTypeNotValidComponent SpeciesTypeNotValidComponent @endlink</code></td>
<td>The SpeciesType object is not supported in SBML Level 1</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#StoichiometryMathNotValidComponent StoichiometryMathNotValidComponent @endlink</code></td>
<td>The StoichiometryMath object is not supported in SBML Level 1</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#MultiplierNotValidAttribute MultiplierNotValidAttribute @endlink</code></td>
<td>Attribute 'multiplier' on Unit objects is not supported in SBML Level 1</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#OffsetNotValidAttribute OffsetNotValidAttribute @endlink</code></td>
<td>Attribute 'offset' on Unit objects is only available in SBML Level 2 Version 1</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td></tr>
<tr><td><code>@link SBMLErrorCode_t#L3SpatialDimensionsUnset L3SpatialDimensionsUnset @endlink</code></td>
<td>No value given for 'spatialDimensions' attribute; assuming a value of 3</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link SBMLErrorCode_t#PackageConversionNotSupported PackageConversionNotSupported @endlink</code></td>
<td>Conversion of SBML Level 3 package constructs is not yet supported</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#InvalidTargetLevelVersion InvalidTargetLevelVersion @endlink</code></td>
<td>The requested SBML Level/Version combination is not known to exist</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link SBMLErrorCode_t#L3NotSupported L3NotSupported @endlink</code></td>
<td>SBML Level 3 is not yet supported</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompUnknown CompUnknown @endlink</code></td>
<td> Unknown error from comp </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompNSUndeclared CompNSUndeclared @endlink</code></td>
<td> The comp ns is not correctly declared </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompElementNotInNs CompElementNotInNs @endlink</code></td>
<td> Element not in comp namespace </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompDuplicateComponentId CompDuplicateComponentId @endlink</code></td>
<td> Duplicate 'id' attribute value </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompUniqueModelIds CompUniqueModelIds @endlink</code></td>
<td> Model and ExternalModelDefinitions must have unique ids </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompUniquePortIds CompUniquePortIds @endlink</code></td>
<td> Ports must have unique ids </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompInvalidSIdSyntax CompInvalidSIdSyntax @endlink</code></td>
<td> Invalid SId syntax </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompInvalidSubmodelRefSyntax CompInvalidSubmodelRefSyntax @endlink</code></td>
<td> Invalid submodelRef syntax </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompInvalidDeletionSyntax CompInvalidDeletionSyntax @endlink</code></td>
<td> Invalid deletion syntax </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompInvalidConversionFactorSyntax CompInvalidConversionFactorSyntax @endlink</code></td>
<td> Invalid conversionFactor syntax </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompInvalidNameSyntax CompInvalidNameSyntax @endlink</code></td>
<td> Invalid name syntax </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompReplacedUnitsShouldMatch CompReplacedUnitsShouldMatch @endlink</code></td>
<td> Units of replaced elements should match replacement units. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompOneListOfReplacedElements CompOneListOfReplacedElements @endlink</code></td>
<td> Only one <code>&lt;listOfReplacedElements&gt;</code> allowed. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompLOReplaceElementsAllowedElements CompLOReplaceElementsAllowedElements @endlink</code></td>
<td> Allowed children of <code>&lt;listOfReplacedElements&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompLOReplacedElementsAllowedAttribs CompLOReplacedElementsAllowedAttribs @endlink</code></td>
<td> Allowed <code>&lt;listOfReplacedElements&gt;</code> attributes </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompEmptyLOReplacedElements CompEmptyLOReplacedElements @endlink</code></td>
<td> <code>&lt;listOfReplacedElements&gt;</code> must not be empty </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompOneReplacedByElement CompOneReplacedByElement @endlink</code></td>
<td> Only one <code>&lt;replacedBy&gt;</code> object allowed. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompAttributeRequiredMissing CompAttributeRequiredMissing @endlink</code></td>
<td> Required comp:required attribute on <code>&lt;sbml&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompAttributeRequiredMustBeBoolean CompAttributeRequiredMustBeBoolean @endlink</code></td>
<td> The comp:required attribute must be Boolean </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompRequiredTrueIfElementsRemain CompRequiredTrueIfElementsRemain @endlink</code></td>
<td> The comp:required attribute must be 'true' if math changes. NOTE:  Deprecated </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompRequiredFalseIfAllElementsReplaced CompRequiredFalseIfAllElementsReplaced @endlink</code></td>
<td> The comp:required attribute must be 'false' if math does not change. NOTE:  Deprecated </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompOneListOfModelDefinitions CompOneListOfModelDefinitions @endlink</code></td>
<td> Only one <code>&lt;listOfModelDefinitions&gt;</code> allowed. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompEmptyLOModelDefs CompEmptyLOModelDefs @endlink</code></td>
<td> <code>&lt;listOfModelDefinitions&gt;</code> and <code>&lt;listOfExternalModelDefinitions&gt;</code> must not be empty </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompLOModelDefsAllowedElements CompLOModelDefsAllowedElements @endlink</code></td>
<td> Only <code>&lt;modelDefinitions&gt;</code> in <code>&lt;listOfModelDefinitions&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompLOExtModelDefsAllowedElements CompLOExtModelDefsAllowedElements @endlink</code></td>
<td> Only <code>&lt;externalModelDefinitions&gt;</code> in <code>&lt;listOfExternalModelDefinitions&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompLOModelDefsAllowedAttributes CompLOModelDefsAllowedAttributes @endlink</code></td>
<td> Allowed <code>&lt;listOfModelDefinitions&gt;</code> attributes </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompLOExtModDefsAllowedAttributes CompLOExtModDefsAllowedAttributes @endlink</code></td>
<td> Allowed <code>&lt;listOfExternalModelDefinitions&gt;</code> attributes </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompOneListOfExtModelDefinitions CompOneListOfExtModelDefinitions @endlink</code></td>
<td> Only one <code>&lt;listOfExternalModelDefinitions&gt;</code> allowed. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompAttributeRequiredMustBeTrue CompAttributeRequiredMustBeTrue @endlink</code></td>
<td> The comp:required attribute must be 'true' </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompExtModDefAllowedCoreAttributes CompExtModDefAllowedCoreAttributes @endlink</code></td>
<td> Allowed <code>&lt;externalModelDefinitions&gt;</code> core attributes </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompExtModDefAllowedElements CompExtModDefAllowedElements @endlink</code></td>
<td> Allowed <code>&lt;externalModelDefinitions&gt;</code> elements </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompExtModDefAllowedAttributes CompExtModDefAllowedAttributes @endlink</code></td>
<td> Allowed <code>&lt;externalModelDefinitions&gt;</code> attributes </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompReferenceMustBeL3 CompReferenceMustBeL3 @endlink</code></td>
<td> External models must be L3 </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompModReferenceMustIdOfModel CompModReferenceMustIdOfModel @endlink</code></td>
<td> 'modelRef' must be the 'id' of a model in the 'source' document </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompExtModMd5DoesNotMatch CompExtModMd5DoesNotMatch @endlink</code></td>
<td> MD5 checksum does not match the 'source' document </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompInvalidSourceSyntax CompInvalidSourceSyntax @endlink</code></td>
<td> The 'comp:source' attribute must be of type 'anyURI' </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompInvalidModelRefSyntax CompInvalidModelRefSyntax @endlink</code></td>
<td> The 'comp:modelRef' attribute must have the syntax of 'SId' </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompInvalidMD5Syntax CompInvalidMD5Syntax @endlink</code></td>
<td> The 'comp:md5' attribute must have the syntax of 'string' </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompCircularExternalModelReference CompCircularExternalModelReference @endlink</code></td>
<td> Circular reference in <code>&lt;externalModelDefinition&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompOneListOfOnModel CompOneListOfOnModel @endlink</code></td>
<td> Only one <code>&lt;listOfSubmodels&gt;</code> and one <code>&lt;listOfPorts&gt;</code> allowed </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompNoEmptyListOfOnModel CompNoEmptyListOfOnModel @endlink</code></td>
<td> No empty listOf elements allowed </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompLOSubmodelsAllowedElements CompLOSubmodelsAllowedElements @endlink</code></td>
<td> Allowed elements on <code>&lt;listOfSubmodels&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompLOPortsAllowedElements CompLOPortsAllowedElements @endlink</code></td>
<td> Allowed elements on <code>&lt;listOfPorts&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompLOSubmodelsAllowedAttributes CompLOSubmodelsAllowedAttributes @endlink</code></td>
<td> Allowed attributes on <code>&lt;listOfSubmodels&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompLOPortsAllowedAttributes CompLOPortsAllowedAttributes @endlink</code></td>
<td> Allowed attributes on <code>&lt;listOfPorts&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompSubmodelAllowedCoreAttributes CompSubmodelAllowedCoreAttributes @endlink</code></td>
<td> Allowed core attributes on <code>&lt;submodel&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompSubmodelAllowedElements CompSubmodelAllowedElements @endlink</code></td>
<td> Allowed elements on <code>&lt;submodel&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompOneListOfDeletionOnSubmodel CompOneListOfDeletionOnSubmodel @endlink</code></td>
<td> Only one <code>&lt;listOfDeletions&gt;</code> on a <code>&lt;submodel&gt;</code> allowed </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompSubmodelNoEmptyLODeletions CompSubmodelNoEmptyLODeletions @endlink</code></td>
<td> No empty listOfDeletions elements allowed </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompLODeletionsAllowedElements CompLODeletionsAllowedElements @endlink</code></td>
<td> Allowed elements on <code>&lt;listOfDeletions&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompLODeletionAllowedAttributes CompLODeletionAllowedAttributes @endlink</code></td>
<td> Allowed <code>&lt;listOfDeletions&gt;</code> attributes </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompSubmodelAllowedAttributes CompSubmodelAllowedAttributes @endlink</code></td>
<td> Allowed <code>&lt;submodel&gt;</code> attributes </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompModReferenceSyntax CompModReferenceSyntax @endlink</code></td>
<td> 'comp:modelRef' must conform to SId syntax </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompInvalidTimeConvFactorSyntax CompInvalidTimeConvFactorSyntax @endlink</code></td>
<td> 'comp:timeConversionFactor' must conform to SId syntax </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompInvalidExtentConvFactorSyntax CompInvalidExtentConvFactorSyntax @endlink</code></td>
<td> 'comp:extentConversionFactor' must conform to SId syntax </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompSubmodelMustReferenceModel CompSubmodelMustReferenceModel @endlink</code></td>
<td> The 'comp:modelRef' attribute must reference a model </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompSubmodelCannotReferenceSelf CompSubmodelCannotReferenceSelf @endlink</code></td>
<td> The 'comp:modelRef' attribute cannot reference own model </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompModCannotCircularlyReferenceSelf CompModCannotCircularlyReferenceSelf @endlink</code></td>
<td> <code>&lt;model&gt;</code> may not reference <code>&lt;submodel&gt;</code> that references itself. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompTimeConversionMustBeParameter CompTimeConversionMustBeParameter @endlink</code></td>
<td> The 'comp:timeConversionFactor' must reference a parameter </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompExtentConversionMustBeParameter CompExtentConversionMustBeParameter @endlink</code></td>
<td> The 'comp:extentConversionFactor' must reference a parameter </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompPortRefMustReferencePort CompPortRefMustReferencePort @endlink</code></td>
<td> The 'comp:portRef' attribute must be the 'id' of a <code>&lt;port&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompIdRefMustReferenceObject CompIdRefMustReferenceObject @endlink</code></td>
<td> The 'comp:idRef' attribute must be the 'id' of a model element </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompUnitRefMustReferenceUnitDef CompUnitRefMustReferenceUnitDef @endlink</code></td>
<td> The 'comp:unitRef' attribute must be the 'id' of a UnitDefinition </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompMetaIdRefMustReferenceObject CompMetaIdRefMustReferenceObject @endlink</code></td>
<td> The 'comp:metaIdRef' attribute must be the 'metaid' of an object </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompParentOfSBRefChildMustBeSubmodel CompParentOfSBRefChildMustBeSubmodel @endlink</code></td>
<td> If <code>&lt;sBaseRef&gt;</code> has a child <code>&lt;sBaseRef&gt;</code> its parent must be a <code>&lt;submodel&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompInvalidPortRefSyntax CompInvalidPortRefSyntax @endlink</code></td>
<td> The 'comp:portRef' attribute must have the syntax of an SBML SId </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompInvalidIdRefSyntax CompInvalidIdRefSyntax @endlink</code></td>
<td> The 'comp:idRef' attribute must have the syntax of an SBML SId </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompInvalidUnitRefSyntax CompInvalidUnitRefSyntax @endlink</code></td>
<td> The 'comp:unitRef' attribute must have the syntax of an SBML SId </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompInvalidMetaIdRefSyntax CompInvalidMetaIdRefSyntax @endlink</code></td>
<td> The 'comp:metaIdRef' attribute must have the syntax of an XML ID </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompOneSBaseRefOnly CompOneSBaseRefOnly @endlink</code></td>
<td> Only one <code>&lt;sbaseRef&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompDeprecatedSBaseRefSpelling CompDeprecatedSBaseRefSpelling @endlink</code></td>
<td> The spelling 'sbaseRef' is deprecated </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompSBaseRefMustReferenceObject CompSBaseRefMustReferenceObject @endlink</code></td>
<td> An SBaseRef must reference an object. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompSBaseRefMustReferenceOnlyOneObject CompSBaseRefMustReferenceOnlyOneObject @endlink</code></td>
<td> An SBaseRef must reference only one other object. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompNoMultipleReferences CompNoMultipleReferences @endlink</code></td>
<td> Objects may not be referenced by mutiple SBaseRef constructs. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompPortMustReferenceObject CompPortMustReferenceObject @endlink</code></td>
<td> Port must reference an object </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompPortMustReferenceOnlyOneObject CompPortMustReferenceOnlyOneObject @endlink</code></td>
<td> Port must reference only one other object. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompPortAllowedAttributes CompPortAllowedAttributes @endlink</code></td>
<td> Allowed attributes on a Port </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompPortReferencesUnique CompPortReferencesUnique @endlink</code></td>
<td> Port definitions must be unique. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompDeletionMustReferenceObject CompDeletionMustReferenceObject @endlink</code></td>
<td> Deletion must reference an object </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompDeletionMustReferOnlyOneObject CompDeletionMustReferOnlyOneObject @endlink</code></td>
<td> Deletion must reference only one other object. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompDeletionAllowedAttributes CompDeletionAllowedAttributes @endlink</code></td>
<td> Allowed attributes on a Deletion </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompReplacedElementMustRefObject CompReplacedElementMustRefObject @endlink</code></td>
<td> ReplacedElement must reference an object </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompReplacedElementMustRefOnlyOne CompReplacedElementMustRefOnlyOne @endlink</code></td>
<td> ReplacedElement must reference only one other object. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompReplacedElementAllowedAttributes CompReplacedElementAllowedAttributes @endlink</code></td>
<td> Allowed attributes on <code>&lt;replacedElement&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompReplacedElementSubModelRef CompReplacedElementSubModelRef @endlink</code></td>
<td> The 'comp:submodelRef' attribute must point to a <code>&lt;submodel&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompReplacedElementDeletionRef CompReplacedElementDeletionRef @endlink</code></td>
<td> The 'comp:deletion' attribute must point to a <code>&lt;deletion&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompReplacedElementConvFactorRef CompReplacedElementConvFactorRef @endlink</code></td>
<td> The 'comp:conversionFactor attribute must point to a <code>&lt;parameter&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompReplacedElementSameReference CompReplacedElementSameReference @endlink</code></td>
<td> No <code>&lt;replacedElement&gt;</code> refer to same object </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompReplacedElementNoDelAndConvFact CompReplacedElementNoDelAndConvFact @endlink</code></td>
<td> No <code>&lt;replacedElement&gt;</code> with deletion and conversionfactor </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompReplacedByMustRefObject CompReplacedByMustRefObject @endlink</code></td>
<td> ReplacedBy must reference an object </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompReplacedByMustRefOnlyOne CompReplacedByMustRefOnlyOne @endlink</code></td>
<td> ReplacedBy must reference only one other object. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompReplacedByAllowedAttributes CompReplacedByAllowedAttributes @endlink</code></td>
<td> Allowed attributes on <code>&lt;replacedBy&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompReplacedBySubModelRef CompReplacedBySubModelRef @endlink</code></td>
<td> The 'comp:submodelRef' attribute must point to a <code>&lt;submodel&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompMustReplaceSameClass CompMustReplaceSameClass @endlink</code></td>
<td> Replaced classes must match. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompMustReplaceIDs CompMustReplaceIDs @endlink</code></td>
<td> Replaced IDs must be replaced with IDs. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompMustReplaceMetaIDs CompMustReplaceMetaIDs @endlink</code></td>
<td> Replaced metaids must be replaced with metaids. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompMustReplacePackageIDs CompMustReplacePackageIDs @endlink</code></td>
<td> Replaced package IDs must be replaced with package IDs. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompUnresolvedReference CompUnresolvedReference @endlink</code></td>
<td> Unresolved reference. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompNoModelInReference CompNoModelInReference @endlink</code></td>
<td> No model in referenced document. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompExtModDefBad CompExtModDefBad @endlink</code></td>
<td> Referenced <code>&lt;externalModelDefinition&gt;</code> unresolvable. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompModelFlatteningFailed CompModelFlatteningFailed @endlink</code></td>
<td> Model failed to flatten. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompFlatModelNotValid CompFlatModelNotValid @endlink</code></td>
<td> Flat model not valid. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompLineNumbersUnreliable CompLineNumbersUnreliable @endlink</code></td>
<td> Line numbers unreliable. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompFlatteningNotRecognisedReqd CompFlatteningNotRecognisedReqd @endlink</code></td>
<td> Flattening not implemented for required package. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompFlatteningNotRecognisedNotReqd CompFlatteningNotRecognisedNotReqd @endlink</code></td>
<td> Flattening not implemented for unrequired package. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompFlatteningNotImplementedNotReqd CompFlatteningNotImplementedNotReqd @endlink</code></td>
<td> Flattening not implemented for unrequired package. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompFlatteningNotImplementedReqd CompFlatteningNotImplementedReqd @endlink</code></td>
<td> Flattening not implemented for required package. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompFlatteningWarning CompFlatteningWarning @endlink</code></td>
<td> Flattening reference may come from package. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompDeprecatedDeleteFunction CompDeprecatedDeleteFunction @endlink</code></td>
<td> The performDeletions functions is deprecated. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompDeprecatedReplaceFunction CompDeprecatedReplaceFunction @endlink</code></td>
<td> The performReplacementsAndConversions fuctions is deprecated. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompDeletedReplacement CompDeletedReplacement @endlink</code></td>
<td> Element deleted before a subelement could be replaced. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompIdRefMayReferenceUnknownPackage CompIdRefMayReferenceUnknownPackage @endlink</code></td>
<td> The 'comp:idRef' attribute must be the 'id' of a model element </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link CompSBMLErrorCode_t#CompMetaIdRefMayReferenceUnknownPkg CompMetaIdRefMayReferenceUnknownPkg @endlink</code></td>
<td> The 'comp:metaIdRef' attribute must be the 'metaid' of a model element </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcUnknown FbcUnknown @endlink</code></td>
<td> Unknown error from fbc </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcNSUndeclared FbcNSUndeclared @endlink</code></td>
<td> The fbc ns is not correctly declared </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcElementNotInNs FbcElementNotInNs @endlink</code></td>
<td> Element not in fbc namespace </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcDuplicateComponentId FbcDuplicateComponentId @endlink</code></td>
<td> Duplicate 'id' attribute value </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcSBMLSIdSyntax FbcSBMLSIdSyntax @endlink</code></td>
<td> Invalid 'id' attribute </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcAttributeRequiredMissing FbcAttributeRequiredMissing @endlink</code></td>
<td> Required fbc:required attribute on <code>&lt;sbml&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcAttributeRequiredMustBeBoolean FbcAttributeRequiredMustBeBoolean @endlink</code></td>
<td> The fbc:required attribute must be Boolean </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcRequiredFalse FbcRequiredFalse @endlink</code></td>
<td> The fbc:required attribute must be 'false' </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcOnlyOneEachListOf FbcOnlyOneEachListOf @endlink</code></td>
<td> One of each list of allowed </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcNoEmptyListOfs FbcNoEmptyListOfs @endlink</code></td>
<td> ListOf elements cannot be empty </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcLOFluxBoundsAllowedElements FbcLOFluxBoundsAllowedElements @endlink</code></td>
<td> Allowed elements on ListOfFluxBounds </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcLOObjectivesAllowedElements FbcLOObjectivesAllowedElements @endlink</code></td>
<td> Allowed elements on ListOfObjectives </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcLOFluxBoundsAllowedAttributes FbcLOFluxBoundsAllowedAttributes @endlink</code></td>
<td> Allowed attributes on ListOfFluxBounds </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcLOObjectivesAllowedAttributes FbcLOObjectivesAllowedAttributes @endlink</code></td>
<td> Allowed attributes on ListOfObjectives </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcActiveObjectiveSyntax FbcActiveObjectiveSyntax @endlink</code></td>
<td> Type of activeObjective attribute </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcActiveObjectiveRefersObjective FbcActiveObjectiveRefersObjective @endlink</code></td>
<td> ActiveObjective must reference Objective </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcSpeciesAllowedL3Attributes FbcSpeciesAllowedL3Attributes @endlink</code></td>
<td> Species allowed attributes </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcSpeciesChargeMustBeInteger FbcSpeciesChargeMustBeInteger @endlink</code></td>
<td> Charge must be integer </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcSpeciesFormulaMustBeString FbcSpeciesFormulaMustBeString @endlink</code></td>
<td> Chemical formula must be string </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxBoundAllowedL3Attributes FbcFluxBoundAllowedL3Attributes @endlink</code></td>
<td> <code>&lt;fluxBound&gt;</code> may only have 'metaId' and 'sboTerm' from L3 namespace </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxBoundAllowedElements FbcFluxBoundAllowedElements @endlink</code></td>
<td> <code>&lt;fluxBound&gt;</code> may only have <code>&lt;notes&gt;</code> and <code>&lt;annotations&gt;</code> from L3 Core </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxBoundRequiredAttributes FbcFluxBoundRequiredAttributes @endlink</code></td>
<td> Invalid attribute found on <code>&lt;fluxBound&gt;</code> object </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxBoundRectionMustBeSIdRef FbcFluxBoundRectionMustBeSIdRef @endlink</code></td>
<td> Datatype for 'fbc:reaction' must be SIdRef </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxBoundNameMustBeString FbcFluxBoundNameMustBeString @endlink</code></td>
<td> The attribute 'fbc:name' must be of the data type string </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxBoundOperationMustBeEnum FbcFluxBoundOperationMustBeEnum @endlink</code></td>
<td> The attribute 'fbc:operation' must be of data type FbcOperation </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxBoundValueMustBeDouble FbcFluxBoundValueMustBeDouble @endlink</code></td>
<td> The attribute 'fbc:value' must be of the data type double </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxBoundReactionMustExist FbcFluxBoundReactionMustExist @endlink</code></td>
<td> 'fbc:reaction' must refer to valid reaction </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxBoundsForReactionConflict FbcFluxBoundsForReactionConflict @endlink</code></td>
<td> Conflicting set of FluxBounds for a reaction </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcObjectiveAllowedL3Attributes FbcObjectiveAllowedL3Attributes @endlink</code></td>
<td> <code>&lt;objective&gt;</code> may only have 'metaId' and 'sboTerm' from L3 namespace </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcObjectiveAllowedElements FbcObjectiveAllowedElements @endlink</code></td>
<td> <code>&lt;objective&gt;</code> may only have <code>&lt;notes&gt;</code> and <code>&lt;annotations&gt;</code> from L3 Core </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcObjectiveRequiredAttributes FbcObjectiveRequiredAttributes @endlink</code></td>
<td> Invalid attribute found on <code>&lt;objective&gt;</code> object </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcObjectiveNameMustBeString FbcObjectiveNameMustBeString @endlink</code></td>
<td> The attribute 'fbc:name' must be of the data type string </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcObjectiveTypeMustBeEnum FbcObjectiveTypeMustBeEnum @endlink</code></td>
<td> The attribute 'fbc:type' must be of data type FbcType. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcObjectiveOneListOfObjectives FbcObjectiveOneListOfObjectives @endlink</code></td>
<td> An <code>&lt;objective&gt;</code> must have one <code>&lt;listOfFluxObjectives&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcObjectiveLOFluxObjMustNotBeEmpty FbcObjectiveLOFluxObjMustNotBeEmpty @endlink</code></td>
<td> <code>&lt;listOfFluxObjectives&gt;</code> subobject must not be empty </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcObjectiveLOFluxObjOnlyFluxObj FbcObjectiveLOFluxObjOnlyFluxObj @endlink</code></td>
<td> Invalid element found in <code>&lt;listOfFluxObjectives&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcObjectiveLOFluxObjAllowedAttribs FbcObjectiveLOFluxObjAllowedAttribs @endlink</code></td>
<td> <code>&lt;listOfFluxObjectives&gt;</code> may only have 'metaId' and 'sboTerm' from L3 core </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxObjectAllowedL3Attributes FbcFluxObjectAllowedL3Attributes @endlink</code></td>
<td> <code>&lt;fluxObjective&gt;</code> may only have 'metaId' and 'sboTerm' from L3 namespace </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxObjectAllowedElements FbcFluxObjectAllowedElements @endlink</code></td>
<td> <code>&lt;fluxObjective&gt;</code> may only have <code>&lt;notes&gt;</code> and <code>&lt;annotations&gt;</code> from L3 Core </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxObjectRequiredAttributes FbcFluxObjectRequiredAttributes @endlink</code></td>
<td> Invalid attribute found on <code>&lt;fluxObjective&gt;</code> object </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxObjectNameMustBeString FbcFluxObjectNameMustBeString @endlink</code></td>
<td> The attribute 'fbc:name' must be of the data type string </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxObjectReactionMustBeSIdRef FbcFluxObjectReactionMustBeSIdRef @endlink</code></td>
<td> Datatype for 'fbc:reaction' must be SIdRef </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxObjectReactionMustExist FbcFluxObjectReactionMustExist @endlink</code></td>
<td> 'fbc:reaction' must refer to valid reaction </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link FbcSBMLErrorCode_t#FbcFluxObjectCoefficientMustBeDouble FbcFluxObjectCoefficientMustBeDouble @endlink</code></td>
<td> The attribute 'fbc:coefficient' must be of the data type double </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutUnknownError LayoutUnknownError @endlink</code></td>
<td> Unknown error from layout </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutNSUndeclared LayoutNSUndeclared @endlink</code></td>
<td> The layout ns is not correctly declared </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutElementNotInNs LayoutElementNotInNs @endlink</code></td>
<td> Element not in layout namespace </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutDuplicateComponentId LayoutDuplicateComponentId @endlink</code></td>
<td> Duplicate 'id' attribute value </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSIdSyntax LayoutSIdSyntax @endlink</code></td>
<td> 'id' attribute incorrect syntax </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutXsiTypeAllowedLocations LayoutXsiTypeAllowedLocations @endlink</code></td>
<td> 'xsi:type' allowed locations </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutXsiTypeSyntax LayoutXsiTypeSyntax @endlink</code></td>
<td> 'xsi:type' attribute incorrect syntax </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutAttributeRequiredMissing LayoutAttributeRequiredMissing @endlink</code></td>
<td> Required layout:required attribute on <code>&lt;sbml&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutAttributeRequiredMustBeBoolean LayoutAttributeRequiredMustBeBoolean @endlink</code></td>
<td> The layout:required attribute must be Boolean </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutRequiredFalse LayoutRequiredFalse @endlink</code></td>
<td> The layout:required attribute must be 'false' </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutOnlyOneLOLayouts LayoutOnlyOneLOLayouts @endlink</code></td>
<td> Only one listOfLayouts on <code>&lt;model&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOLayoutsNotEmpty LayoutLOLayoutsNotEmpty @endlink</code></td>
<td> ListOf elements cannot be empty </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOLayoutsAllowedElements LayoutLOLayoutsAllowedElements @endlink</code></td>
<td> Allowed elements on ListOfLayouts </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOLayoutsAllowedAttributes LayoutLOLayoutsAllowedAttributes @endlink</code></td>
<td> Allowed attributes on ListOfLayouts </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLayoutAllowedElements LayoutLayoutAllowedElements @endlink</code></td>
<td> Allowed elements on Layout </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLayoutAllowedCoreAttributes LayoutLayoutAllowedCoreAttributes @endlink</code></td>
<td> Allowed core attributes on Layout </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutOnlyOneEachListOf LayoutOnlyOneEachListOf @endlink</code></td>
<td> Only one each listOf on <code>&lt;layout&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutNoEmptyListOfs LayoutNoEmptyListOfs @endlink</code></td>
<td> ListOf elements cannot be empty </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLayoutAllowedAttributes LayoutLayoutAllowedAttributes @endlink</code></td>
<td> <code>&lt;layout&gt;</code> must have 'id' and may have 'name' </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLayoutNameMustBeString LayoutLayoutNameMustBeString @endlink</code></td>
<td> 'name' must be string </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOCompGlyphAllowedAttributes LayoutLOCompGlyphAllowedAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;listOfCompartmentGlyphs&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOCompGlyphAllowedElements LayoutLOCompGlyphAllowedElements @endlink</code></td>
<td> Elements allowed on <code>&lt;listOfCompartmentGlyphs&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOSpeciesGlyphAllowedAttributes LayoutLOSpeciesGlyphAllowedAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;listOfSpeciesGlyphs&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOSpeciesGlyphAllowedElements LayoutLOSpeciesGlyphAllowedElements @endlink</code></td>
<td> Elements allowed on <code>&lt;listOfSpeciesGlyphs&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLORnGlyphAllowedAttributes LayoutLORnGlyphAllowedAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;listOfReactionGlyphs&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLORnGlyphAllowedElements LayoutLORnGlyphAllowedElements @endlink</code></td>
<td> Elements allowed on <code>&lt;listOfReactionGlyphs&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOAddGOAllowedAttribut LayoutLOAddGOAllowedAttribut @endlink</code></td>
<td> Attributes allowed on <code>&lt;listOfAdditionalGraphicalObjectGlyphs&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOAddGOAllowedElements LayoutLOAddGOAllowedElements @endlink</code></td>
<td> Elements allowed on <code>&lt;listOfAdditionalGraphicalObjectGlyphs&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLayoutMustHaveDimensions LayoutLayoutMustHaveDimensions @endlink</code></td>
<td> Layout must have <code>&lt;dimensions&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOTextGlyphAllowedAttributes LayoutLOTextGlyphAllowedAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;listOfTextGlyphs&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOTextGlyphAllowedElements LayoutLOTextGlyphAllowedElements @endlink</code></td>
<td> Elements allowed on <code>&lt;listOfTextGlyphs&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGOAllowedCoreElements LayoutGOAllowedCoreElements @endlink</code></td>
<td> Core elements allowed on <code>&lt;graphicalObject&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGOAllowedCoreAttributes LayoutGOAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;graphicalObject&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGOAllowedElements LayoutGOAllowedElements @endlink</code></td>
<td> %Layout elements allowed on <code>&lt;graphicalObject&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGOAllowedAttributes LayoutGOAllowedAttributes @endlink</code></td>
<td> %Layout attributes allowed on <code>&lt;graphicalObject&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGOMetaIdRefMustBeIDREF LayoutGOMetaIdRefMustBeIDREF @endlink</code></td>
<td> Layout 'metIdRef' must be IDREF. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGOMetaIdRefMustReferenceObject LayoutGOMetaIdRefMustReferenceObject @endlink</code></td>
<td> Layout 'metIdRef' must reference existing object. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGOMustContainBoundingBox LayoutGOMustContainBoundingBox @endlink</code></td>
<td> A <code>&lt;graphicalObject&gt;</code> must contain a <code>&lt;boundingBox&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCGAllowedCoreElements LayoutCGAllowedCoreElements @endlink</code></td>
<td> Core elements allowed on <code>&lt;compartmentGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCGAllowedCoreAttributes LayoutCGAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;compartmentGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCGAllowedElements LayoutCGAllowedElements @endlink</code></td>
<td> %Layout elements allowed on <code>&lt;compartmentGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCGAllowedAttributes LayoutCGAllowedAttributes @endlink</code></td>
<td> %Layout attributes allowed on <code>&lt;compartmentGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCGMetaIdRefMustBeIDREF LayoutCGMetaIdRefMustBeIDREF @endlink</code></td>
<td> Layout 'metIdRef' must be IDREF. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCGMetaIdRefMustReferenceObject LayoutCGMetaIdRefMustReferenceObject @endlink</code></td>
<td> Layout 'metIdRef' must reference existing object. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCGCompartmentSyntax LayoutCGCompartmentSyntax @endlink</code></td>
<td> CompartmentGlyph 'compartment' must have SIdRef syntax. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCGCompartmentMustRefComp LayoutCGCompartmentMustRefComp @endlink</code></td>
<td> CompartmentGlyph compartment must reference existing compartment. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCGNoDuplicateReferences LayoutCGNoDuplicateReferences @endlink</code></td>
<td> CompartmentGlyph cannot reference two objects. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCGOrderMustBeDouble LayoutCGOrderMustBeDouble @endlink</code></td>
<td> CompartmentGlyph order must be double. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSGAllowedCoreElements LayoutSGAllowedCoreElements @endlink</code></td>
<td> Core elements allowed on <code>&lt;speciesGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSGAllowedCoreAttributes LayoutSGAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;speciesGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSGAllowedElements LayoutSGAllowedElements @endlink</code></td>
<td> %Layout elements allowed on <code>&lt;speciesGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSGAllowedAttributes LayoutSGAllowedAttributes @endlink</code></td>
<td> %Layout attributes allowed on <code>&lt;speciesGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSGMetaIdRefMustBeIDREF LayoutSGMetaIdRefMustBeIDREF @endlink</code></td>
<td> Layout 'metIdRef' must be IDREF. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSGMetaIdRefMustReferenceObject LayoutSGMetaIdRefMustReferenceObject @endlink</code></td>
<td> Layout 'metIdRef' must reference existing object. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSGSpeciesSyntax LayoutSGSpeciesSyntax @endlink</code></td>
<td> SpeciesGlyph 'species' must have SIdRef syntax. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSGSpeciesMustRefSpecies LayoutSGSpeciesMustRefSpecies @endlink</code></td>
<td> SpeciesGlyph species must reference existing species. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSGNoDuplicateReferences LayoutSGNoDuplicateReferences @endlink</code></td>
<td> SpeciesGlyph cannot reference two objects. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutRGAllowedCoreElements LayoutRGAllowedCoreElements @endlink</code></td>
<td> Core elements allowed on <code>&lt;reactionGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutRGAllowedCoreAttributes LayoutRGAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;reactionGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutRGAllowedElements LayoutRGAllowedElements @endlink</code></td>
<td> %Layout elements allowed on <code>&lt;reactionGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutRGAllowedAttributes LayoutRGAllowedAttributes @endlink</code></td>
<td> %Layout attributes allowed on <code>&lt;reactionGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutRGMetaIdRefMustBeIDREF LayoutRGMetaIdRefMustBeIDREF @endlink</code></td>
<td> Layout 'metIdRef' must be IDREF. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutRGMetaIdRefMustReferenceObject LayoutRGMetaIdRefMustReferenceObject @endlink</code></td>
<td> Layout 'metIdRef' must reference existing object. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutRGReactionSyntax LayoutRGReactionSyntax @endlink</code></td>
<td> ReactionGlyph 'reaction' must have SIdRef syntax. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutRGReactionMustRefReaction LayoutRGReactionMustRefReaction @endlink</code></td>
<td> ReactionGlyph reaction must reference existing reaction. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutRGNoDuplicateReferences LayoutRGNoDuplicateReferences @endlink</code></td>
<td> ReactionGlyph cannot reference two objects. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOSpeciesRefGlyphAllowedElements LayoutLOSpeciesRefGlyphAllowedElements @endlink</code></td>
<td> Allowed elements on ListOfSpeciesReferenceGlyphs </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOSpeciesRefGlyphAllowedAttribs LayoutLOSpeciesRefGlyphAllowedAttribs @endlink</code></td>
<td> Allowed attributes on ListOfSpeciesReferenceGlyphs </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOSpeciesRefGlyphNotEmpty LayoutLOSpeciesRefGlyphNotEmpty @endlink</code></td>
<td> ListOfSpeciesReferenceGlyphs not empty </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGGAllowedCoreElements LayoutGGAllowedCoreElements @endlink</code></td>
<td> Core elements allowed on <code>&lt;generalGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGGAllowedCoreAttributes LayoutGGAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;generalGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGGAllowedElements LayoutGGAllowedElements @endlink</code></td>
<td> %Layout elements allowed on <code>&lt;generalGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGGAllowedAttributes LayoutGGAllowedAttributes @endlink</code></td>
<td> %Layout attributes allowed on <code>&lt;generalGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGGMetaIdRefMustBeIDREF LayoutGGMetaIdRefMustBeIDREF @endlink</code></td>
<td> Layout 'metIdRef' must be IDREF. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGGMetaIdRefMustReferenceObject LayoutGGMetaIdRefMustReferenceObject @endlink</code></td>
<td> Layout 'metIdRef' must reference existing object. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGGReferenceSyntax LayoutGGReferenceSyntax @endlink</code></td>
<td> GeneralGlyph 'reference' must have SIdRef syntax. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGGReferenceMustRefObject LayoutGGReferenceMustRefObject @endlink</code></td>
<td> GeneralGlyph 'reference' must reference existing element. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutGGNoDuplicateReferences LayoutGGNoDuplicateReferences @endlink</code></td>
<td> GeneralGlyph cannot reference two objects. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOReferenceGlyphAllowedElements LayoutLOReferenceGlyphAllowedElements @endlink</code></td>
<td> Allowed elements on ListOfReferenceGlyphs </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOReferenceGlyphAllowedAttribs LayoutLOReferenceGlyphAllowedAttribs @endlink</code></td>
<td> Allowed attributes on ListOfReferenceGlyphs </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOSubGlyphAllowedElements LayoutLOSubGlyphAllowedElements @endlink</code></td>
<td> Allowed elements on ListOfSubGlyphs </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOSubGlyphAllowedAttribs LayoutLOSubGlyphAllowedAttribs @endlink</code></td>
<td> Allowed attributes on ListOfSubGlyphs </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutTGAllowedCoreElements LayoutTGAllowedCoreElements @endlink</code></td>
<td> Core elements allowed on <code>&lt;textGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutTGAllowedCoreAttributes LayoutTGAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;textGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutTGAllowedElements LayoutTGAllowedElements @endlink</code></td>
<td> %Layout elements allowed on <code>&lt;textGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutTGAllowedAttributes LayoutTGAllowedAttributes @endlink</code></td>
<td> %Layout attributes allowed on <code>&lt;textGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutTGMetaIdRefMustBeIDREF LayoutTGMetaIdRefMustBeIDREF @endlink</code></td>
<td> Layout 'metIdRef' must be IDREF. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutTGMetaIdRefMustReferenceObject LayoutTGMetaIdRefMustReferenceObject @endlink</code></td>
<td> Layout 'metIdRef' must reference existing object. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutTGOriginOfTextSyntax LayoutTGOriginOfTextSyntax @endlink</code></td>
<td> TextGlyph 'originOfText' must have SIdRef syntax. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutTGOriginOfTextMustRefObject LayoutTGOriginOfTextMustRefObject @endlink</code></td>
<td> TextGlyph 'originOfText' must reference existing element. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutTGNoDuplicateReferences LayoutTGNoDuplicateReferences @endlink</code></td>
<td> TextGlyph cannot reference two objects. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutTGGraphicalObjectSyntax LayoutTGGraphicalObjectSyntax @endlink</code></td>
<td> TextGlyph 'graphicalObject' must have SIdRef syntax. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutTGGraphicalObjectMustRefObject LayoutTGGraphicalObjectMustRefObject @endlink</code></td>
<td> TextGlyph 'graphicalObject' must reference existing element. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutTGTextMustBeString LayoutTGTextMustBeString @endlink</code></td>
<td> TextGlyph 'text' must be string. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSRGAllowedCoreElements LayoutSRGAllowedCoreElements @endlink</code></td>
<td> Core elements allowed on <code>&lt;speciesReferenceGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSRGAllowedCoreAttributes LayoutSRGAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;speciesReferenceGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSRGAllowedElements LayoutSRGAllowedElements @endlink</code></td>
<td> %Layout elements allowed on <code>&lt;speciesReferenceGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSRGAllowedAttributes LayoutSRGAllowedAttributes @endlink</code></td>
<td> %Layout attributes allowed on <code>&lt;speciesReferenceGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSRGMetaIdRefMustBeIDREF LayoutSRGMetaIdRefMustBeIDREF @endlink</code></td>
<td> Layout 'metIdRef' must be IDREF. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSRGMetaIdRefMustReferenceObject LayoutSRGMetaIdRefMustReferenceObject @endlink</code></td>
<td> Layout 'metIdRef' must reference existing object. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSRGSpeciesReferenceSyntax LayoutSRGSpeciesReferenceSyntax @endlink</code></td>
<td> SpeciesReferenceGlyph 'speciesReference' must have SIdRef syntax. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSRGSpeciesRefMustRefObject LayoutSRGSpeciesRefMustRefObject @endlink</code></td>
<td> SpeciesReferenceGlyph 'speciesReference' must reference existing element. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSRGNoDuplicateReferences LayoutSRGNoDuplicateReferences @endlink</code></td>
<td> SpeciesReferenceGlyph cannot reference two objects. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSRGSpeciesGlyphSyntax LayoutSRGSpeciesGlyphSyntax @endlink</code></td>
<td> SpeciesReferenceGlyph 'speciesGlyph' must have SIdRef syntax. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSRGSpeciesGlyphMustRefObject LayoutSRGSpeciesGlyphMustRefObject @endlink</code></td>
<td> SpeciesReferenceGlyph 'speciesGlyph' must reference existing element. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutSRGRoleSyntax LayoutSRGRoleSyntax @endlink</code></td>
<td> SpeciesReferenceGlyph 'role' must be string from enumeration. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutREFGAllowedCoreElements LayoutREFGAllowedCoreElements @endlink</code></td>
<td> Core elements allowed on <code>&lt;referenceGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutREFGAllowedCoreAttributes LayoutREFGAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;referenceGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutREFGAllowedElements LayoutREFGAllowedElements @endlink</code></td>
<td> %Layout elements allowed on <code>&lt;referenceGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutREFGAllowedAttributes LayoutREFGAllowedAttributes @endlink</code></td>
<td> %Layout attributes allowed on <code>&lt;referenceGlyph&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutREFGMetaIdRefMustBeIDREF LayoutREFGMetaIdRefMustBeIDREF @endlink</code></td>
<td> Layout 'metIdRef' must be IDREF. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutREFGMetaIdRefMustReferenceObject LayoutREFGMetaIdRefMustReferenceObject @endlink</code></td>
<td> Layout 'metIdRef' must reference existing object. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutREFGReferenceSyntax LayoutREFGReferenceSyntax @endlink</code></td>
<td> ReferenceGlyph 'reference' must have SIdRef syntax. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutREFGReferenceMustRefObject LayoutREFGReferenceMustRefObject @endlink</code></td>
<td> ReferenceGlyph 'reference' must reference existing element. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutREFGNoDuplicateReferences LayoutREFGNoDuplicateReferences @endlink</code></td>
<td> ReferenceGlyph cannot reference two objects. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutREFGGlyphSyntax LayoutREFGGlyphSyntax @endlink</code></td>
<td> ReferenceGlyph 'glyph' must have SIdRef syntax. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutREFGGlyphMustRefObject LayoutREFGGlyphMustRefObject @endlink</code></td>
<td> ReferenceGlyph 'glyph' must reference existing element. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutREFGRoleSyntax LayoutREFGRoleSyntax @endlink</code></td>
<td> ReferenceGlyph 'role' must be string. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutPointAllowedCoreElements LayoutPointAllowedCoreElements @endlink</code></td>
<td> Core elements allowed on <code>&lt;point&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutPointAllowedCoreAttributes LayoutPointAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;point&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutPointAllowedAttributes LayoutPointAllowedAttributes @endlink</code></td>
<td> %Layout attributes allowed on <code>&lt;point&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutPointAttributesMustBeDouble LayoutPointAttributesMustBeDouble @endlink</code></td>
<td> Layout 'x', 'y' and 'z' must be double. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutBBoxAllowedCoreElements LayoutBBoxAllowedCoreElements @endlink</code></td>
<td> Core elements allowed on <code>&lt;boundingBox&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutBBoxAllowedCoreAttributes LayoutBBoxAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;boundingBox&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutBBoxAllowedElements LayoutBBoxAllowedElements @endlink</code></td>
<td> %Layout elements allowed on <code>&lt;boundingBox&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutBBoxAllowedAttributes LayoutBBoxAllowedAttributes @endlink</code></td>
<td> %Layout attributes allowed on <code>&lt;boundingBox&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutBBoxConsistent3DDefinition LayoutBBoxConsistent3DDefinition @endlink</code></td>
<td> Layout consistent dimensions on a <code>&lt;boundingBox&gt;</code> </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCurveAllowedCoreElements LayoutCurveAllowedCoreElements @endlink</code></td>
<td> Core elements allowed on <code>&lt;curve&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCurveAllowedCoreAttributes LayoutCurveAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;curve&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCurveAllowedElements LayoutCurveAllowedElements @endlink</code></td>
<td> %Layout elements allowed on <code>&lt;curve&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCurveAllowedAttributes LayoutCurveAllowedAttributes @endlink</code></td>
<td> %Layout attributes allowed on <code>&lt;curve&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOCurveSegsAllowedAttributes LayoutLOCurveSegsAllowedAttributes @endlink</code></td>
<td> Allowed attributes on ListOfCurveSegments </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOCurveSegsAllowedElements LayoutLOCurveSegsAllowedElements @endlink</code></td>
<td> Allowed elements on ListOfCurveSegments </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLOCurveSegsNotEmpty LayoutLOCurveSegsNotEmpty @endlink</code></td>
<td> No empty ListOfCurveSegments </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLSegAllowedCoreElements LayoutLSegAllowedCoreElements @endlink</code></td>
<td> Core elements allowed on <code>&lt;lineSegment&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLSegAllowedCoreAttributes LayoutLSegAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;lineSegment&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLSegAllowedElements LayoutLSegAllowedElements @endlink</code></td>
<td> %Layout elements allowed on <code>&lt;lineSegment&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutLSegAllowedAttributes LayoutLSegAllowedAttributes @endlink</code></td>
<td> %Layout attributes allowed on <code>&lt;lineSegment&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCBezAllowedCoreElements LayoutCBezAllowedCoreElements @endlink</code></td>
<td> Core elements allowed on <code>&lt;cubicBezier&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCBezAllowedCoreAttributes LayoutCBezAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;cubicBezier&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCBezAllowedElements LayoutCBezAllowedElements @endlink</code></td>
<td> %Layout elements allowed on <code>&lt;cubicBezier&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutCBezAllowedAttributes LayoutCBezAllowedAttributes @endlink</code></td>
<td> %Layout attributes allowed on <code>&lt;cubicBezier&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutDimsAllowedCoreElements LayoutDimsAllowedCoreElements @endlink</code></td>
<td> Core elements allowed on <code>&lt;dimensions&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutDimsAllowedCoreAttributes LayoutDimsAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;dimensions&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutDimsAllowedAttributes LayoutDimsAllowedAttributes @endlink</code></td>
<td> %Layout attributes allowed on <code>&lt;dimensions&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link LayoutSBMLErrorCode_t#LayoutDimsAttributesMustBeDouble LayoutDimsAttributesMustBeDouble @endlink</code></td>
<td> Layout 'width', 'height' and 'depth' must be double. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualUnknown QualUnknown @endlink</code></td>
<td> Unknown error from qual. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualNSUndeclared QualNSUndeclared @endlink</code></td>
<td> The qual ns is not correctly declared. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualElementNotInNs QualElementNotInNs @endlink</code></td>
<td> Element not in qual namespace. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualFunctionTermBool QualFunctionTermBool @endlink</code></td>
<td> FunctionTerm should return boolean. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualMathCSymbolDisallowed QualMathCSymbolDisallowed @endlink</code></td>
<td> CSymbol time or delay not allowed. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-warning">W</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualDuplicateComponentId QualDuplicateComponentId @endlink</code></td>
<td> Duplicate 'id' attribute value. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualAttributeRequiredMissing QualAttributeRequiredMissing @endlink</code></td>
<td> Required qual:required attribute on <code>&lt;sbml&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualAttributeRequiredMustBeBoolean QualAttributeRequiredMustBeBoolean @endlink</code></td>
<td> The qual:required attribute must be Boolean. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualRequiredTrueIfTransitions QualRequiredTrueIfTransitions @endlink</code></td>
<td> The qual:required attribute must be 'true' if math changes. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualOneListOfTransOrQS QualOneListOfTransOrQS @endlink</code></td>
<td> Only one <code>&lt;listOfTransitions&gt;</code> or <code>&lt;listOfQualitativeSpecies&gt;</code> allowed. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualEmptyLONotAllowed QualEmptyLONotAllowed @endlink</code></td>
<td> Empty <code>&lt;listOfTransitions&gt;</code> or <code>&lt;listOfQualitativeSpecies&gt;</code> not allowed. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualLOTransitiondAllowedElements QualLOTransitiondAllowedElements @endlink</code></td>
<td> Elements allowed on <code>&lt;listOfTransitions&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualLOQualSpeciesAllowedElements QualLOQualSpeciesAllowedElements @endlink</code></td>
<td> Elements allowed on <code>&lt;listOfTransitions&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualLOQualSpeciesAllowedAttributes QualLOQualSpeciesAllowedAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;listOfQualitativeSpecies&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualLOTransitionsAllowedAttributes QualLOTransitionsAllowedAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;listOfTransitions&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualQualSpeciesAllowedCoreAttributes QualQualSpeciesAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;qualitativeSpecies&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualQualSpeciesAllowedElements QualQualSpeciesAllowedElements @endlink</code></td>
<td> Elements allowed on <code>&lt;qualitativeSpecies&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualQualSpeciesAllowedAttributes QualQualSpeciesAllowedAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;qualitativeSpecies&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualConstantMustBeBool QualConstantMustBeBool @endlink</code></td>
<td> Attribute 'constant' on <code>&lt;qualitativeSpecies&gt;</code> must be boolean. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualNameMustBeString QualNameMustBeString @endlink</code></td>
<td> Attribute 'name' on <code>&lt;qualitativeSpecies&gt;</code> must be string. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualInitialLevelMustBeInt QualInitialLevelMustBeInt @endlink</code></td>
<td> Attribute 'initialLevel' on <code>&lt;qualitativeSpecies&gt;</code> must be integer. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualMaxLevelMustBeInt QualMaxLevelMustBeInt @endlink</code></td>
<td> Attribute 'maxLevel' on <code>&lt;qualitativeSpecies&gt;</code> must be integer. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualCompartmentMustReferExisting QualCompartmentMustReferExisting @endlink</code></td>
<td> Attribute 'compartment' on <code>&lt;qualitativeSpecies&gt;</code> must reference compartment. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualInitialLevelCannotExceedMax QualInitialLevelCannotExceedMax @endlink</code></td>
<td> Attribute 'initialLevel' on <code>&lt;qualitativeSpecies&gt;</code> cannot exceed maxLevel. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualConstantQSCannotBeOutput QualConstantQSCannotBeOutput @endlink</code></td>
<td> Constant <code>&lt;qualitativeSpecies&gt;</code> cannot be an Output. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualQSAssignedOnlyOnce QualQSAssignedOnlyOnce @endlink</code></td>
<td> A <code>&lt;qualitativeSpecies&gt;</code> can only be assigned once. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualInitalLevelNotNegative QualInitalLevelNotNegative @endlink</code></td>
<td> Attribute 'initialLevel' on <code>&lt;qualitativeSpecies&gt;</code> cannot be negative. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualMaxLevelNotNegative QualMaxLevelNotNegative @endlink</code></td>
<td> Attribute 'maxLevel' on <code>&lt;qualitativeSpecies&gt;</code> cannot be negative. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualTransitionAllowedCoreAttributes QualTransitionAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;transition&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualTransitionAllowedElements QualTransitionAllowedElements @endlink</code></td>
<td> Elements allowed on <code>&lt;transition&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualTransitionAllowedAttributes QualTransitionAllowedAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;transition&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualTransitionNameMustBeString QualTransitionNameMustBeString @endlink</code></td>
<td> Attribute 'name' on <code>&lt;transition&gt;</code> must be string. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualTransitionLOElements QualTransitionLOElements @endlink</code></td>
<td> ListOf elements on <code>&lt;transition&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualTransitionEmptyLOElements QualTransitionEmptyLOElements @endlink</code></td>
<td> ListOf elements on <code>&lt;transition&gt;</code> not empty. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualTransitionLOInputElements QualTransitionLOInputElements @endlink</code></td>
<td> Elements on <code>&lt;listOfInputs&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualTransitionLOOutputElements QualTransitionLOOutputElements @endlink</code></td>
<td> Elements on <code>&lt;listOfOutputs&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualTransitionLOFuncTermElements QualTransitionLOFuncTermElements @endlink</code></td>
<td> Elements on <code>&lt;listOfFunctionTerms&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualTransitionLOInputAttributes QualTransitionLOInputAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;listOfInputs&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualTransitionLOOutputAttributes QualTransitionLOOutputAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;listOfOutputs&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualTransitionLOFuncTermAttributes QualTransitionLOFuncTermAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;listOfFunctionTerms&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualTransitionLOFuncTermExceedMax QualTransitionLOFuncTermExceedMax @endlink</code></td>
<td> <code>&lt;listOfFunctionTerms&gt;</code> cannot make qualitativeSpecies exceed maxLevel. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualTransitionLOFuncTermNegative QualTransitionLOFuncTermNegative @endlink</code></td>
<td> <code>&lt;listOfFunctionTerms&gt;</code> cannot make qualitativeSpecies negative. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualInputAllowedCoreAttributes QualInputAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;input&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualInputAllowedElements QualInputAllowedElements @endlink</code></td>
<td> Elements allowed on <code>&lt;input&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualInputAllowedAttributes QualInputAllowedAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;input&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualInputNameMustBeString QualInputNameMustBeString @endlink</code></td>
<td> Attribute 'name' on <code>&lt;input&gt;</code> must be string. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualInputSignMustBeSignEnum QualInputSignMustBeSignEnum @endlink</code></td>
<td> Attribute 'sign' on <code>&lt;input&gt;</code> must be enum. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualInputTransEffectMustBeInputEffect QualInputTransEffectMustBeInputEffect @endlink</code></td>
<td> Attribute 'transitionEffect' on <code>&lt;input&gt;</code> must be enum. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualInputThreshMustBeInteger QualInputThreshMustBeInteger @endlink</code></td>
<td> Attribute 'thresholdLevel' on <code>&lt;input&gt;</code> must be non negative integer. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualInputQSMustBeExistingQS QualInputQSMustBeExistingQS @endlink</code></td>
<td> Attribute 'qualitativeSpecies' on <code>&lt;input&gt;</code> must refer to existing. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualInputConstantCannotBeConsumed QualInputConstantCannotBeConsumed @endlink</code></td>
<td> Constant <code>&lt;input&gt;</code> cannot be consumed. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualInputThreshMustBeNonNegative QualInputThreshMustBeNonNegative @endlink</code></td>
<td> Attribute 'thresholdLevel' on <code>&lt;input&gt;</code> must be non negative integer. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualOutputAllowedCoreAttributes QualOutputAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;output&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualOutputAllowedElements QualOutputAllowedElements @endlink</code></td>
<td> Elements allowed on <code>&lt;output&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualOutputAllowedAttributes QualOutputAllowedAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;output&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualOutputNameMustBeString QualOutputNameMustBeString @endlink</code></td>
<td> Attribute 'name' on <code>&lt;output&gt;</code> must be string. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualOutputTransEffectMustBeOutput QualOutputTransEffectMustBeOutput @endlink</code></td>
<td> Attribute 'transitionEffect' on <code>&lt;output&gt;</code> must be enum. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualOutputLevelMustBeInteger QualOutputLevelMustBeInteger @endlink</code></td>
<td> Attribute 'outputLevel' on <code>&lt;output&gt;</code> must be non negative integer. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualOutputQSMustBeExistingQS QualOutputQSMustBeExistingQS @endlink</code></td>
<td> Attribute 'qualitativeSpecies' on <code>&lt;output&gt;</code> must refer to existing. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualOutputConstantMustBeFalse QualOutputConstantMustBeFalse @endlink</code></td>
<td> Constant 'qualitativeSpecies' cannot be <code>&lt;output&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualOutputProductionMustHaveLevel QualOutputProductionMustHaveLevel @endlink</code></td>
<td> <code>&lt;output&gt;</code> being produced must have level. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualOutputLevelMustBeNonNegative QualOutputLevelMustBeNonNegative @endlink</code></td>
<td> Attribute 'outputLevel' on <code>&lt;output&gt;</code> must be non negative integer. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualDefaultTermAllowedCoreAttributes QualDefaultTermAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;defaultTerm&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualDefaultTermAllowedElements QualDefaultTermAllowedElements @endlink</code></td>
<td> Elements allowed on <code>&lt;defaultTerm&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualDefaultTermAllowedAttributes QualDefaultTermAllowedAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;defaultTerm&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualDefaultTermResultMustBeInteger QualDefaultTermResultMustBeInteger @endlink</code></td>
<td> Attribute 'resultLevel' on <code>&lt;defaultTerm&gt;</code> must be non negative integer. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualDefaultTermResultMustBeNonNeg QualDefaultTermResultMustBeNonNeg @endlink</code></td>
<td> Attribute 'resultLevel' on <code>&lt;defaultTerm&gt;</code> must be non negative integer. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualFuncTermAllowedCoreAttributes QualFuncTermAllowedCoreAttributes @endlink</code></td>
<td> Core attributes allowed on <code>&lt;functionTerm&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualFuncTermAllowedElements QualFuncTermAllowedElements @endlink</code></td>
<td> Elements allowed on <code>&lt;functionTerm&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualFuncTermAllowedAttributes QualFuncTermAllowedAttributes @endlink</code></td>
<td> Attributes allowed on <code>&lt;functionTerm&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualFuncTermOnlyOneMath QualFuncTermOnlyOneMath @endlink</code></td>
<td> Only one <code>&lt;math&gt;</code> on <code>&lt;functionTerm&gt;</code>. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualFuncTermResultMustBeInteger QualFuncTermResultMustBeInteger @endlink</code></td>
<td> Attribute 'resultLevel' on <code>&lt;functionTerm&gt;</code> must be non negative integer. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
<tr><td><code>@link QualSBMLErrorCode_t#QualFuncTermResultMustBeNonNeg QualFuncTermResultMustBeNonNeg @endlink</code></td>
<td> Attribute 'resultLevel' on <code>&lt;functionTerm&gt;</code> must be non negative integer.. </td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-na">N</td>
<td class="s-error">E</td></tr>
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
 */
