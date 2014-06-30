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
 * and read by
 * @if clike SBML_formulaToString()@endif@if csharp
 * SBML_formulaToString()@endif@if python libsbml.formulaToString()@endif@if java <code><a
 * href="libsbml.html#formulaToString(org.sbml.libsbml.ASTNode)">libsbml.formulaToString(ASTNode
 * tree)</a></code>@endif@~ and
 * @if clike SBML_parseFormula()@endif@if csharp
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
 * To maintain backwards compatibility for libSBML users, the original
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
 * The following lists the main differences in the formula syntax supported by
 * the "Level&nbsp;3" or L3 versions of the formula parsers and formatters,
 * compared to what is supported by
 * @if clike SBML_parseFormula()@endif@if csharp
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
 * produce a <code>&lt;piecewise&gt;</code> function in the corresponding
 * MathML output.
 *
 * @li All inverse trigonometric functions may be defined in the infix either
 * using @c arc as a prefix or simply @c a; in other words, both @c arccsc
 * and @c acsc are interpreted as the operator @em arccosecant as defined in
 * MathML&nbsp;2.0.  (Many functions in the simpler SBML Level&nbsp;1 oriented
 * parser implemented by
 * @if clike SBML_parseFormula()@endif@if csharp
 * SBML_parseFormula()@endif@if python libsbml.parseFormula()@endif@if java
 * <code><a
 * href="libsbml.html#parseFormula(java.lang.String)">libsbml.parseFormula(String
 * formula)</a></code>@endif@~ are defined this way as well, but not all.)
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
 * @if clike SBML_parseL3FormulaWithSettings()@endif@if csharp
 * SBML_parseL3FormulaWithSettings()@endif@if python
 * libsbml.parseL3FormulaWithSettings()@endif@if java <code><a
 * href="libsbml.html#parseL3FormulaWithSettings(java.lang.String,
 * org.sbml.libsbml.L3ParserSettings)">libsbml.parseL3FormulaWithSettings(String
 * formula, L3ParserSettings settings)</a></code>@endif@~ and @if clike
 * SBML_formulaToL3String()@endif@if csharp SBML_formulaL3ToString()@endif@if python
 * libsbml.formulaToL3String()@endif@if java <code><a
 * href="libsbml.html#formulaToL3String(org.sbml.libsbml.ASTNode)">libsbml.formulaToL3String(ASTNode
 * tree)</a></code>@endif@~. The settings available include the following:
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
 * @if clike SBML_parseL3FormulaWithModel()@endif@if csharp
 * SBML_parseL3FormulaWithModel()@endif@if python
 * libsbml.parseL3FormulaWithModel()@endif@if java <code><a
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
 * <code>&lt;pi/&gt;</code>.
 *
 * <li style="margin-bottom: 0.5em"> Similarly, when a Model object is
 * provided, @c SId values of user-defined functions present in the model
 * will be used preferentially over pre-defined MathML functions.  For
 * example, if the passed-in Model contains a FunctionDefinition object with
 * the identifier &quot;<code>sin</code>&quot;, that function will be used
 * instead of the predefined MathML function <code>&lt;sin/&gt;</code>.
 *
 * </ul>
 *
 * These configuration settings cannot be changed directly using the basic
 * parser and formatter functions, but can be changed on a per-call basis
 * by using the alternative functions @if clike
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
 * Note that this facility (and the SBML Level&nbsp;1-oriented @if clike
 * SBML_parseFormula()@endif@if csharp SBML_parseFormula()@endif@if python
 * libsbml.parseFormula()@endif@if java <code><a
 * href="libsbml.html#parseFormula(java.lang.String)">libsbml.parseFormula(String
 * formula)</a></code>@endif) are provided as a convenience by
 * libSBML&mdash;neither SBML nor the MathML standard define a "string-form"
 * equivalent to MathML expressions, so the choice of formula syntax is
 * arbitrary.  The approach taken by libSBML is to start with the syntax
 * defined by SBML Level&nbsp;1 (which in fact used a custom text-string
 * representation of formulas, and not MathML), and expand it to include the
 * functionality described above.  This formula syntax is based mostly on C
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
<table id="sbmlerror-table" class="text-table small-font alt-row-colors"
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
<tr><td class="code">@link SBMLErrorCode_t#XMLUnknownError XMLUnknownError@endlink</td>
<td class="meaning">Unknown error</td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLOutOfMemory XMLOutOfMemory@endlink</td>
<td class="meaning">Out of memory</td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLFileUnreadable XMLFileUnreadable@endlink</td>
<td class="meaning">File unreadable</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLFileUnwritable XMLFileUnwritable@endlink</td>
<td class="meaning">File unwritable</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLFileOperationError XMLFileOperationError@endlink</td>
<td class="meaning">File operation error</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLNetworkAccessError XMLNetworkAccessError@endlink</td>
<td class="meaning">Network access error</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InternalXMLParserError InternalXMLParserError@endlink</td>
<td class="meaning">Internal XML parser error</td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UnrecognizedXMLParserCode UnrecognizedXMLParserCode@endlink</td>
<td class="meaning">Unrecognized XML parser code</td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLTranscoderError XMLTranscoderError@endlink</td>
<td class="meaning">Transcoder error</td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MissingXMLDecl MissingXMLDecl@endlink</td>
<td class="meaning">Missing XML declaration</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MissingXMLEncoding MissingXMLEncoding@endlink</td>
<td class="meaning">Missing XML encoding attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadXMLDecl BadXMLDecl@endlink</td>
<td class="meaning">Bad XML declaration</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadXMLDOCTYPE BadXMLDOCTYPE@endlink</td>
<td class="meaning">Bad XML DOCTYPE</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidCharInXML InvalidCharInXML@endlink</td>
<td class="meaning">Invalid character</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadlyFormedXML BadlyFormedXML@endlink</td>
<td class="meaning">Badly formed XML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UnclosedXMLToken UnclosedXMLToken@endlink</td>
<td class="meaning">Unclosed token</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidXMLConstruct InvalidXMLConstruct@endlink</td>
<td class="meaning">Invalid XML construct</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLTagMismatch XMLTagMismatch@endlink</td>
<td class="meaning">XML tag mismatch</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DuplicateXMLAttribute DuplicateXMLAttribute@endlink</td>
<td class="meaning">Duplicate attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UndefinedXMLEntity UndefinedXMLEntity@endlink</td>
<td class="meaning">Undefined XML entity</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadProcessingInstruction BadProcessingInstruction@endlink</td>
<td class="meaning">Bad XML processing instruction</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadXMLPrefix BadXMLPrefix@endlink</td>
<td class="meaning">Bad XML prefix</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadXMLPrefixValue BadXMLPrefixValue@endlink</td>
<td class="meaning">Bad XML prefix value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MissingXMLRequiredAttribute MissingXMLRequiredAttribute@endlink</td>
<td class="meaning">Missing required attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLAttributeTypeMismatch XMLAttributeTypeMismatch@endlink</td>
<td class="meaning">Attribute type mismatch</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLBadUTF8Content XMLBadUTF8Content@endlink</td>
<td class="meaning">Bad UTF8 content</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MissingXMLAttributeValue MissingXMLAttributeValue@endlink</td>
<td class="meaning">Missing attribute value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadXMLAttributeValue BadXMLAttributeValue@endlink</td>
<td class="meaning">Bad attribute value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadXMLAttribute BadXMLAttribute@endlink</td>
<td class="meaning">Bad XML attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UnrecognizedXMLElement UnrecognizedXMLElement@endlink</td>
<td class="meaning">Unrecognized XML element</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadXMLComment BadXMLComment@endlink</td>
<td class="meaning">Bad XML comment</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadXMLDeclLocation BadXMLDeclLocation@endlink</td>
<td class="meaning">Bad XML declaration location</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLUnexpectedEOF XMLUnexpectedEOF@endlink</td>
<td class="meaning">Unexpected EOF</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadXMLIDValue BadXMLIDValue@endlink</td>
<td class="meaning">Bad XML ID value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadXMLIDRef BadXMLIDRef@endlink</td>
<td class="meaning">Bad XML IDREF</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UninterpretableXMLContent UninterpretableXMLContent@endlink</td>
<td class="meaning">Uninterpretable XML content</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadXMLDocumentStructure BadXMLDocumentStructure@endlink</td>
<td class="meaning">Bad XML document structure</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidAfterXMLContent InvalidAfterXMLContent@endlink</td>
<td class="meaning">Invalid content after XML content</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLExpectedQuotedString XMLExpectedQuotedString@endlink</td>
<td class="meaning">Expected quoted string</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLEmptyValueNotPermitted XMLEmptyValueNotPermitted@endlink</td>
<td class="meaning">Empty value not permitted</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLBadNumber XMLBadNumber@endlink</td>
<td class="meaning">Bad number</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLBadColon XMLBadColon@endlink</td>
<td class="meaning">Colon character not permitted</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MissingXMLElements MissingXMLElements@endlink</td>
<td class="meaning">Missing XML elements</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#XMLContentEmpty XMLContentEmpty@endlink</td>
<td class="meaning">Empty XML content</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UnknownError UnknownError@endlink</td>
<td class="meaning">Encountered unknown internal libSBML error</td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
<td class="s-fatal"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NotUTF8 NotUTF8@endlink</td>
<td class="meaning">File does not use UTF-8 encoding</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UnrecognizedElement UnrecognizedElement@endlink</td>
<td class="meaning">Encountered unrecognized element</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NotSchemaConformant NotSchemaConformant@endlink</td>
<td class="meaning">Document does not conform to the SBML XML schema</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#L3NotSchemaConformant L3NotSchemaConformant@endlink</td>
<td class="meaning">Document is not well-formed XML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidMathElement InvalidMathElement@endlink</td>
<td class="meaning">Invalid MathML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DisallowedMathMLSymbol DisallowedMathMLSymbol@endlink</td>
<td class="meaning">Disallowed MathML symbol found</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DisallowedMathMLEncodingUse DisallowedMathMLEncodingUse@endlink</td>
<td class="meaning">Use of the MathML 'encoding' attribute is not allowed on this element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DisallowedDefinitionURLUse DisallowedDefinitionURLUse@endlink</td>
<td class="meaning">Use of the MathML 'definitionURL' attribute is not allowed on this element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadCsymbolDefinitionURLValue BadCsymbolDefinitionURLValue@endlink</td>
<td class="meaning">Invalid <code>&lt;csymbol&gt;</code> 'definitionURL' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DisallowedMathTypeAttributeUse DisallowedMathTypeAttributeUse@endlink</td>
<td class="meaning">Use of the MathML 'type' attribute is not allowed on this element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DisallowedMathTypeAttributeValue DisallowedMathTypeAttributeValue@endlink</td>
<td class="meaning">Disallowed MathML 'type' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LambdaOnlyAllowedInFunctionDef LambdaOnlyAllowedInFunctionDef@endlink</td>
<td class="meaning">Use of <code>&lt;lambda&gt;</code> not permitted outside of FunctionDefinition objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BooleanOpsNeedBooleanArgs BooleanOpsNeedBooleanArgs@endlink</td>
<td class="meaning">Non-Boolean argument given to Boolean operator</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NumericOpsNeedNumericArgs NumericOpsNeedNumericArgs@endlink</td>
<td class="meaning">Non-numerical argument given to numerical operator</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ArgsToEqNeedSameType ArgsToEqNeedSameType@endlink</td>
<td class="meaning">Arguments to <code>&lt;eq&gt;</code> and <code>&lt;neq&gt;</code> must have the same data types</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#PiecewiseNeedsConsistentTypes PiecewiseNeedsConsistentTypes@endlink</td>
<td class="meaning">Terms in a <code>&lt;piecewise&gt;</code> expression must have consistent data types</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#PieceNeedsBoolean PieceNeedsBoolean@endlink</td>
<td class="meaning">The second argument of a <code>&lt;piece&gt;</code> expression must yield a Boolean value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ApplyCiMustBeUserFunction ApplyCiMustBeUserFunction@endlink</td>
<td class="meaning">A <code>&lt;ci&gt;</code> element in this context must refer to a function definition</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ApplyCiMustBeModelComponent ApplyCiMustBeModelComponent@endlink</td>
<td class="meaning">A <code>&lt;ci&gt;</code> element in this context must refer to a model component</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#KineticLawParametersAreLocalOnly KineticLawParametersAreLocalOnly@endlink</td>
<td class="meaning">Cannot use a KineticLaw local parameter outside of its local scope</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MathResultMustBeNumeric MathResultMustBeNumeric@endlink</td>
<td class="meaning">A formula's result in this context must be a numerical value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OpsNeedCorrectNumberOfArgs OpsNeedCorrectNumberOfArgs@endlink</td>
<td class="meaning">Incorrect number of arguments given to MathML operator</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidNoArgsPassedToFunctionDef InvalidNoArgsPassedToFunctionDef@endlink</td>
<td class="meaning">Incorrect number of arguments given to function invocation</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DisallowedMathUnitsUse DisallowedMathUnitsUse@endlink</td>
<td class="meaning">Attribute 'units' is only permitted on <code>&lt;cn&gt;</code> elements</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidUnitsValue InvalidUnitsValue@endlink</td>
<td class="meaning">Invalid value given for the 'units' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DuplicateComponentId DuplicateComponentId@endlink</td>
<td class="meaning">Duplicate 'id' attribute value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DuplicateUnitDefinitionId DuplicateUnitDefinitionId@endlink</td>
<td class="meaning">Duplicate unit definition 'id' attribute value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DuplicateLocalParameterId DuplicateLocalParameterId@endlink</td>
<td class="meaning">Duplicate local parameter 'id' attribute value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MultipleAssignmentOrRateRules MultipleAssignmentOrRateRules@endlink</td>
<td class="meaning">Multiple rules for the same variable are not allowed</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MultipleEventAssignmentsForId MultipleEventAssignmentsForId@endlink</td>
<td class="meaning">Multiple event assignments for the same variable are not allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#EventAndAssignmentRuleForId EventAndAssignmentRuleForId@endlink</td>
<td class="meaning">An event assignment and an assignment rule must not have the same value for 'variable'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DuplicateMetaId DuplicateMetaId@endlink</td>
<td class="meaning">Duplicate 'metaid' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidSBOTermSyntax InvalidSBOTermSyntax@endlink</td>
<td class="meaning">Invalid syntax for an 'sboTerm' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidMetaidSyntax InvalidMetaidSyntax@endlink</td>
<td class="meaning">Invalid syntax for a 'metaid' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidIdSyntax InvalidIdSyntax@endlink</td>
<td class="meaning">Invalid syntax for an 'id' attribute value</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidUnitIdSyntax InvalidUnitIdSyntax@endlink</td>
<td class="meaning">Invalid syntax for the identifier of a unit</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidNameSyntax InvalidNameSyntax@endlink</td>
<td class="meaning">Invalid syntax for a 'name' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MissingAnnotationNamespace MissingAnnotationNamespace@endlink</td>
<td class="meaning">Missing declaration of the XML namespace for the annotation</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DuplicateAnnotationNamespaces DuplicateAnnotationNamespaces@endlink</td>
<td class="meaning">Multiple annotations using the same XML namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SBMLNamespaceInAnnotation SBMLNamespaceInAnnotation@endlink</td>
<td class="meaning">The SBML XML namespace cannot be used in an Annotation object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MultipleAnnotations MultipleAnnotations@endlink</td>
<td class="meaning">Only one Annotation object is permitted under a given SBML object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InconsistentArgUnits InconsistentArgUnits@endlink</td>
<td class="meaning">The units of the function call's arguments are not consistent with its definition</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InconsistentKineticLawUnitsL3 InconsistentKineticLawUnitsL3@endlink</td>
<td class="meaning">The kinetic law's units are inconsistent with those of other kinetic laws in the model</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AssignRuleCompartmentMismatch AssignRuleCompartmentMismatch@endlink</td>
<td class="meaning">Mismatched units in assignment rule for compartment</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AssignRuleSpeciesMismatch AssignRuleSpeciesMismatch@endlink</td>
<td class="meaning">Mismatched units in assignment rule for species</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AssignRuleParameterMismatch AssignRuleParameterMismatch@endlink</td>
<td class="meaning">Mismatched units in assignment rule for parameter</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AssignRuleStoichiometryMismatch AssignRuleStoichiometryMismatch@endlink</td>
<td class="meaning">Mismatched units in assignment rule for stoichiometry</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InitAssignCompartmenMismatch InitAssignCompartmenMismatch@endlink</td>
<td class="meaning">Mismatched units in initial assignment to compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InitAssignSpeciesMismatch InitAssignSpeciesMismatch@endlink</td>
<td class="meaning">Mismatched units in initial assignment to species</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InitAssignParameterMismatch InitAssignParameterMismatch@endlink</td>
<td class="meaning">Mismatched units in initial assignment to parameter</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InitAssignStoichiometryMismatch InitAssignStoichiometryMismatch@endlink</td>
<td class="meaning">Mismatched units in initial assignment to stoichiometry</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#RateRuleCompartmentMismatch RateRuleCompartmentMismatch@endlink</td>
<td class="meaning">Mismatched units in rate rule for compartment</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#RateRuleSpeciesMismatch RateRuleSpeciesMismatch@endlink</td>
<td class="meaning">Mismatched units in rate rule for species</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#RateRuleParameterMismatch RateRuleParameterMismatch@endlink</td>
<td class="meaning">Mismatched units in rate rule for parameter</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#RateRuleStoichiometryMismatch RateRuleStoichiometryMismatch@endlink</td>
<td class="meaning">Mismatched units in rate rule for stoichiometry</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#KineticLawNotSubstancePerTime KineticLawNotSubstancePerTime@endlink</td>
<td class="meaning">The units of the kinetic law are not 'substance'/'time'</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SpeciesInvalidExtentUnits SpeciesInvalidExtentUnits@endlink</td>
<td class="meaning">The species' units are not consistent with units of extent</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DelayUnitsNotTime DelayUnitsNotTime@endlink</td>
<td class="meaning">The units of the delay expression are not units of time</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#EventAssignCompartmentMismatch EventAssignCompartmentMismatch@endlink</td>
<td class="meaning">Mismatched units in event assignment for compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#EventAssignSpeciesMismatch EventAssignSpeciesMismatch@endlink</td>
<td class="meaning">Mismatched units in event assignment for species</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#EventAssignParameterMismatch EventAssignParameterMismatch@endlink</td>
<td class="meaning">Mismatched units in event assignment for parameter</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#EventAssignStoichiometryMismatch EventAssignStoichiometryMismatch@endlink</td>
<td class="meaning">Mismatched units in event assignment for stoichiometry</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#PriorityUnitsNotDimensionless PriorityUnitsNotDimensionless@endlink</td>
<td class="meaning">The units of a priority expression must be 'dimensionless'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OverdeterminedSystem OverdeterminedSystem@endlink</td>
<td class="meaning">The model is overdetermined</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidModelSBOTerm InvalidModelSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a Model object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidFunctionDefSBOTerm InvalidFunctionDefSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a FunctionDefinition object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidParameterSBOTerm InvalidParameterSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a Parameter object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidInitAssignSBOTerm InvalidInitAssignSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for an InitialAssignment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidRuleSBOTerm InvalidRuleSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a Rule object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidConstraintSBOTerm InvalidConstraintSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a Constraint object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidReactionSBOTerm InvalidReactionSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a Reaction object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidSpeciesReferenceSBOTerm InvalidSpeciesReferenceSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a SpeciesReference object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidKineticLawSBOTerm InvalidKineticLawSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a KineticLaw object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidEventSBOTerm InvalidEventSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for an Event object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidEventAssignmentSBOTerm InvalidEventAssignmentSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for an EventAssignment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidCompartmentSBOTerm InvalidCompartmentSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a Compartment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidSpeciesSBOTerm InvalidSpeciesSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a Species object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidCompartmentTypeSBOTerm InvalidCompartmentTypeSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a CompartmentType object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidSpeciesTypeSBOTerm InvalidSpeciesTypeSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for a SpeciesType object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidTriggerSBOTerm InvalidTriggerSBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for an Event Trigger object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidDelaySBOTerm InvalidDelaySBOTerm@endlink</td>
<td class="meaning">Invalid 'sboTerm' attribute value for an Event Delay object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NotesNotInXHTMLNamespace NotesNotInXHTMLNamespace@endlink</td>
<td class="meaning">Notes must be placed in the XHTML XML namespace</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NotesContainsXMLDecl NotesContainsXMLDecl@endlink</td>
<td class="meaning">XML declarations are not permitted in Notes objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NotesContainsDOCTYPE NotesContainsDOCTYPE@endlink</td>
<td class="meaning">XML DOCTYPE elements are not permitted in Notes objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidNotesContent InvalidNotesContent@endlink</td>
<td class="meaning">Invalid notes content found</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyOneNotesElementAllowed OnlyOneNotesElementAllowed@endlink</td>
<td class="meaning">Only one Notes subobject is permitted on a given SBML object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidNamespaceOnSBML InvalidNamespaceOnSBML@endlink</td>
<td class="meaning">Invalid XML namespace for the SBML container element</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MissingOrInconsistentLevel MissingOrInconsistentLevel@endlink</td>
<td class="meaning">Missing or inconsistent value for the 'level' attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MissingOrInconsistentVersion MissingOrInconsistentVersion@endlink</td>
<td class="meaning">Missing or inconsistent value for the 'version' attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#PackageNSMustMatch PackageNSMustMatch@endlink</td>
<td class="meaning">Inconsistent or invalid SBML Level/Version for the package namespace declaration</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LevelPositiveInteger LevelPositiveInteger@endlink</td>
<td class="meaning">The 'level' attribute must have a positive integer value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#VersionPositiveInteger VersionPositiveInteger@endlink</td>
<td class="meaning">The 'version' attribute must have a positive integer value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnSBML AllowedAttributesOnSBML@endlink</td>
<td class="meaning">Invalid attribute found on the SBML container element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#L3PackageOnLowerSBML L3PackageOnLowerSBML@endlink</td>
<td class="meaning">An L3 package ns found on the SBML container element</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MissingModel MissingModel@endlink</td>
<td class="meaning">No model definition found</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#IncorrectOrderInModel IncorrectOrderInModel@endlink</td>
<td class="meaning">Incorrect ordering of components within the Model object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#EmptyListElement EmptyListElement@endlink</td>
<td class="meaning">Empty ListOf___ object found</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NeedCompartmentIfHaveSpecies NeedCompartmentIfHaveSpecies@endlink</td>
<td class="meaning">The presence of a species requires a compartment</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneOfEachListOf OneOfEachListOf@endlink</td>
<td class="meaning">Only one of each kind of ListOf___ object is allowed inside a Model object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyFuncDefsInListOfFuncDefs OnlyFuncDefsInListOfFuncDefs@endlink</td>
<td class="meaning">Only FunctionDefinition, Notes and Annotation objects are allowed in ListOfFunctionDefinitions</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyUnitDefsInListOfUnitDefs OnlyUnitDefsInListOfUnitDefs@endlink</td>
<td class="meaning">Only UnitDefinition, Notes and Annotation objects are allowed in ListOfUnitDefinitions objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyCompartmentsInListOfCompartments OnlyCompartmentsInListOfCompartments@endlink</td>
<td class="meaning">Only Compartment, Notes and Annotation objects are allowed in ListOfCompartments objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlySpeciesInListOfSpecies OnlySpeciesInListOfSpecies@endlink</td>
<td class="meaning">Only Species, Notes and Annotation objects are allowed in ListOfSpecies objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyParametersInListOfParameters OnlyParametersInListOfParameters@endlink</td>
<td class="meaning">Only Parameter, Notes and Annotation objects are allowed in ListOfParameters objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyInitAssignsInListOfInitAssigns OnlyInitAssignsInListOfInitAssigns@endlink</td>
<td class="meaning">Only InitialAssignment, Notes and Annotation objects are allowed in ListOfInitialAssignments objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyRulesInListOfRules OnlyRulesInListOfRules@endlink</td>
<td class="meaning">Only Rule, Notes and Annotation objects are allowed in ListOfRules objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyConstraintsInListOfConstraints OnlyConstraintsInListOfConstraints@endlink</td>
<td class="meaning">Only Constraint, Notes and Annotation objects are allowed in ListOfConstraints objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyReactionsInListOfReactions OnlyReactionsInListOfReactions@endlink</td>
<td class="meaning">Only Reaction, Notes and Annotation objects are allowed in ListOfReactions objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyEventsInListOfEvents OnlyEventsInListOfEvents@endlink</td>
<td class="meaning">Only Event, Notes and Annotation objects are allowed in ListOfEvents objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#L3ConversionFactorOnModel L3ConversionFactorOnModel@endlink</td>
<td class="meaning">A 'conversionFactor' attribute value must reference a Parameter object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#L3TimeUnitsOnModel L3TimeUnitsOnModel@endlink</td>
<td class="meaning">Invalid 'timeUnits' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#L3VolumeUnitsOnModel L3VolumeUnitsOnModel@endlink</td>
<td class="meaning">Invalid 'volumeUnits' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#L3AreaUnitsOnModel L3AreaUnitsOnModel@endlink</td>
<td class="meaning">Invalid 'areaUnits' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#L3LengthUnitsOnModel L3LengthUnitsOnModel@endlink</td>
<td class="meaning">Invalid 'lengthUnits' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#L3ExtentUnitsOnModel L3ExtentUnitsOnModel@endlink</td>
<td class="meaning">Invalid 'extentUnits' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnModel AllowedAttributesOnModel@endlink</td>
<td class="meaning">Invalid attribute found on the Model object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfFuncs AllowedAttributesOnListOfFuncs@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfFunctionDefinitions object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfUnitDefs AllowedAttributesOnListOfUnitDefs@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfUnitDefinitions object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfComps AllowedAttributesOnListOfComps@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfCompartments object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfSpecies AllowedAttributesOnListOfSpecies@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfSpecies object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfParams AllowedAttributesOnListOfParams@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfParameters object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfInitAssign AllowedAttributesOnListOfInitAssign@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfInitialAssignments object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfRules AllowedAttributesOnListOfRules@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfRules object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfConstraints AllowedAttributesOnListOfConstraints@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfConstraints object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfReactions AllowedAttributesOnListOfReactions@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfReactions object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfEvents AllowedAttributesOnListOfEvents@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfEvents object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FunctionDefMathNotLambda FunctionDefMathNotLambda@endlink</td>
<td class="meaning">Invalid expression found in the function definition</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidApplyCiInLambda InvalidApplyCiInLambda@endlink</td>
<td class="meaning">Invalid forward reference in the MathML <code>&lt;apply&gt;</code><code>&lt;ci&gt;</code>...<code>&lt;/ci&gt;</code><code>&lt;/apply&gt;</code> expression</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#RecursiveFunctionDefinition RecursiveFunctionDefinition@endlink</td>
<td class="meaning">Recursive function definitions are not permitted</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidCiInLambda InvalidCiInLambda@endlink</td>
<td class="meaning">Invalid <code>&lt;ci&gt;</code> reference found inside the <code>&lt;lambda&gt;</code> mathematical formula</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidFunctionDefReturnType InvalidFunctionDefReturnType@endlink</td>
<td class="meaning">A function's return type must be either a number or a Boolean</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneMathElementPerFunc OneMathElementPerFunc@endlink</td>
<td class="meaning">A FunctionDefinition object must contain one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnFunc AllowedAttributesOnFunc@endlink</td>
<td class="meaning">Invalid attribute found on the FunctionDefinition object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidUnitDefId InvalidUnitDefId@endlink</td>
<td class="meaning">Invalid 'id' attribute value for a UnitDefinition object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidSubstanceRedefinition InvalidSubstanceRedefinition@endlink</td>
<td class="meaning">Invalid redefinition of built-in type 'substance'</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidLengthRedefinition InvalidLengthRedefinition@endlink</td>
<td class="meaning">Invalid redefinition of built-in type 'length'</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidAreaRedefinition InvalidAreaRedefinition@endlink</td>
<td class="meaning">Invalid redefinition of built-in type name 'area'</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidTimeRedefinition InvalidTimeRedefinition@endlink</td>
<td class="meaning">Invalid redefinition of built-in type name 'time'</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidVolumeRedefinition InvalidVolumeRedefinition@endlink</td>
<td class="meaning">Invalid redefinition of built-in type name 'volume'</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#VolumeLitreDefExponentNotOne VolumeLitreDefExponentNotOne@endlink</td>
<td class="meaning">Must use 'exponent'=1 when defining 'volume' in terms of litres</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#VolumeMetreDefExponentNot3 VolumeMetreDefExponentNot3@endlink</td>
<td class="meaning">Must use 'exponent'=3 when defining 'volume' in terms of metres</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#EmptyListOfUnits EmptyListOfUnits@endlink</td>
<td class="meaning">An empty list of Unit objects is not permitted in a UnitDefinition object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidUnitKind InvalidUnitKind@endlink</td>
<td class="meaning">Invalid value for the 'kind' attribute of a UnitDefinition object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OffsetNoLongerValid OffsetNoLongerValid@endlink</td>
<td class="meaning">Unit attribute 'offset' is not supported in this Level+Version of SBML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CelsiusNoLongerValid CelsiusNoLongerValid@endlink</td>
<td class="meaning">Unit name 'Celsius' is not defined in this Level+Version of SBML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#EmptyUnitListElement EmptyUnitListElement@endlink</td>
<td class="meaning">A ListOfUnits object must not be empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneListOfUnitsPerUnitDef OneListOfUnitsPerUnitDef@endlink</td>
<td class="meaning">At most one ListOfUnits object is allowed inside a UnitDefinition object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyUnitsInListOfUnits OnlyUnitsInListOfUnits@endlink</td>
<td class="meaning">Only Unit, Notes and Annotation objects are allowed in ListOfUnits objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnUnitDefinition AllowedAttributesOnUnitDefinition@endlink</td>
<td class="meaning">Invalid attribute found on the UnitDefinition object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfUnits AllowedAttributesOnListOfUnits@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfUnits object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnUnit AllowedAttributesOnUnit@endlink</td>
<td class="meaning">Invalid attribute found on the Unit object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ZeroDimensionalCompartmentSize ZeroDimensionalCompartmentSize@endlink</td>
<td class="meaning">Invalid use of the 'size' attribute for a zero-dimensional compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ZeroDimensionalCompartmentUnits ZeroDimensionalCompartmentUnits@endlink</td>
<td class="meaning">Invalid use of the 'units' attribute for a zero-dimensional compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ZeroDimensionalCompartmentConst ZeroDimensionalCompartmentConst@endlink</td>
<td class="meaning">Zero-dimensional compartments must be defined to be constant</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UndefinedOutsideCompartment UndefinedOutsideCompartment@endlink</td>
<td class="meaning">Invalid value for the 'outside' attribute of a Compartment object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#RecursiveCompartmentContainment RecursiveCompartmentContainment@endlink</td>
<td class="meaning">Recursive nesting of compartments via the 'outside' attribute is not permitted</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ZeroDCompartmentContainment ZeroDCompartmentContainment@endlink</td>
<td class="meaning">Invalid nesting of zero-dimensional compartments</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#Invalid1DCompartmentUnits Invalid1DCompartmentUnits@endlink</td>
<td class="meaning">Invalid value for the 'units' attribute of a one-dimensional compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#Invalid2DCompartmentUnits Invalid2DCompartmentUnits@endlink</td>
<td class="meaning">Invalid value for the 'units' attribute of a two-dimensional compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#Invalid3DCompartmentUnits Invalid3DCompartmentUnits@endlink</td>
<td class="meaning">Invalid value for the 'units' attribute of a three-dimensional compartment</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidCompartmentTypeRef InvalidCompartmentTypeRef@endlink</td>
<td class="meaning">Invalid value for the 'compartmentType' attribute of a compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneDimensionalCompartmentUnits OneDimensionalCompartmentUnits@endlink</td>
<td class="meaning">No units defined for 1-D compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#TwoDimensionalCompartmentUnits TwoDimensionalCompartmentUnits@endlink</td>
<td class="meaning">No units defined for 2-D compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ThreeDimensionalCompartmentUnits ThreeDimensionalCompartmentUnits@endlink</td>
<td class="meaning">No units defined for 3-D Compartment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnCompartment AllowedAttributesOnCompartment@endlink</td>
<td class="meaning">Invalid attribute found on Compartment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoUnitsOnCompartment NoUnitsOnCompartment@endlink</td>
<td class="meaning">No units defined for Compartment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidSpeciesCompartmentRef InvalidSpeciesCompartmentRef@endlink</td>
<td class="meaning">Invalid value found for Species 'compartment' attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#HasOnlySubsNoSpatialUnits HasOnlySubsNoSpatialUnits@endlink</td>
<td class="meaning">Attribute 'spatialSizeUnits' must not be set if 'hasOnlySubstanceUnits'='true'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoSpatialUnitsInZeroD NoSpatialUnitsInZeroD@endlink</td>
<td class="meaning">Attribute 'spatialSizeUnits' must not be set if the compartment is zero-dimensional</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoConcentrationInZeroD NoConcentrationInZeroD@endlink</td>
<td class="meaning">Attribute 'initialConcentration' must not be set if the compartment is zero-dimensional</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SpatialUnitsInOneD SpatialUnitsInOneD@endlink</td>
<td class="meaning">Invalid value for 'spatialSizeUnits' attribute of a one-dimensional compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SpatialUnitsInTwoD SpatialUnitsInTwoD@endlink</td>
<td class="meaning">Invalid value for the 'spatialSizeUnits' attribute of a two-dimensional compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SpatialUnitsInThreeD SpatialUnitsInThreeD@endlink</td>
<td class="meaning">Invalid value for the 'spatialSizeUnits' attribute of a three-dimensional compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidSpeciesSusbstanceUnits InvalidSpeciesSusbstanceUnits@endlink</td>
<td class="meaning">Invalid value for a Species 'units' attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BothAmountAndConcentrationSet BothAmountAndConcentrationSet@endlink</td>
<td class="meaning">Cannot set both 'initialConcentration' and 'initialAmount' attributes simultaneously</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NonBoundarySpeciesAssignedAndUsed NonBoundarySpeciesAssignedAndUsed@endlink</td>
<td class="meaning">Cannot use a non-boundary species in both reactions and rules simultaneously</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NonConstantSpeciesUsed NonConstantSpeciesUsed@endlink</td>
<td class="meaning">Cannot use a constant, non-boundary species as a reactant or product</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidSpeciesTypeRef InvalidSpeciesTypeRef@endlink</td>
<td class="meaning">Invalid value for the 'speciesType' attribute of a species</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MultSpeciesSameTypeInCompartment MultSpeciesSameTypeInCompartment@endlink</td>
<td class="meaning">Cannot have multiple species of the same species type in the same compartment</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MissingSpeciesCompartment MissingSpeciesCompartment@endlink</td>
<td class="meaning">Missing value for the 'compartment' attribute</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SpatialSizeUnitsRemoved SpatialSizeUnitsRemoved@endlink</td>
<td class="meaning">Attribute 'spatialSizeUnits' is not supported in this Level+Version of SBML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SubstanceUnitsOnSpecies SubstanceUnitsOnSpecies@endlink</td>
<td class="meaning">No substance units defined for the species</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ConversionFactorOnSpecies ConversionFactorOnSpecies@endlink</td>
<td class="meaning">Invalid value for the 'conversionFactor' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnSpecies AllowedAttributesOnSpecies@endlink</td>
<td class="meaning">Invalid attribute found on Species object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidParameterUnits InvalidParameterUnits@endlink</td>
<td class="meaning">Invalid value for the 'units' attribute of a Parameter object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ParameterUnits ParameterUnits@endlink</td>
<td class="meaning">No units defined for the parameter</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ConversionFactorMustConstant ConversionFactorMustConstant@endlink</td>
<td class="meaning">A conversion factor must reference a Parameter object declared to be a constant</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnParameter AllowedAttributesOnParameter@endlink</td>
<td class="meaning">Invalid attribute found on Parameter object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidInitAssignSymbol InvalidInitAssignSymbol@endlink</td>
<td class="meaning">Invalid value for the 'symbol' attribute of an InitialAssignment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MultipleInitAssignments MultipleInitAssignments@endlink</td>
<td class="meaning">Multiple initial assignments for the same 'symbol' value are not allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InitAssignmentAndRuleForSameId InitAssignmentAndRuleForSameId@endlink</td>
<td class="meaning">Cannot set a value using both an initial assignment and an assignment rule simultaneously</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneMathElementPerInitialAssign OneMathElementPerInitialAssign@endlink</td>
<td class="meaning">An InitialAssignment object must contain one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnInitialAssign AllowedAttributesOnInitialAssign@endlink</td>
<td class="meaning">Invalid attribute found on an InitialAssignment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidAssignRuleVariable InvalidAssignRuleVariable@endlink</td>
<td class="meaning">Invalid value for the 'variable' attribute of an AssignmentRule object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidRateRuleVariable InvalidRateRuleVariable@endlink</td>
<td class="meaning">Invalid value for the 'variable' attribute of a RateRule object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AssignmentToConstantEntity AssignmentToConstantEntity@endlink</td>
<td class="meaning">An assignment rule cannot assign an entity declared to be constant</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#RateRuleForConstantEntity RateRuleForConstantEntity@endlink</td>
<td class="meaning">A rate rule cannot assign an entity declared to be constant</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CircularRuleDependency CircularRuleDependency@endlink</td>
<td class="meaning">Circular dependencies involving rules and reactions are not permitted</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneMathElementPerRule OneMathElementPerRule@endlink</td>
<td class="meaning">A rule object must contain one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnAssignRule AllowedAttributesOnAssignRule@endlink</td>
<td class="meaning">Invalid attribute found on an AssignmentRule object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnRateRule AllowedAttributesOnRateRule@endlink</td>
<td class="meaning">Invalid attribute found on a RateRule object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnAlgRule AllowedAttributesOnAlgRule@endlink</td>
<td class="meaning">Invalid attribute found on an AlgebraicRule object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ConstraintMathNotBoolean ConstraintMathNotBoolean@endlink</td>
<td class="meaning">A Constraint object's <code>&lt;math&gt;</code> must evaluate to a Boolean value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#IncorrectOrderInConstraint IncorrectOrderInConstraint@endlink</td>
<td class="meaning">Subobjects inside the Constraint object are not in the prescribed order</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ConstraintNotInXHTMLNamespace ConstraintNotInXHTMLNamespace@endlink</td>
<td class="meaning">A Constraint's Message subobject must be in the XHTML XML namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ConstraintContainsXMLDecl ConstraintContainsXMLDecl@endlink</td>
<td class="meaning">XML declarations are not permitted within Constraint's Message objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ConstraintContainsDOCTYPE ConstraintContainsDOCTYPE@endlink</td>
<td class="meaning">XML DOCTYPE elements are not permitted within Constraint's Message objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidConstraintContent InvalidConstraintContent@endlink</td>
<td class="meaning">Invalid content for a Constraint object's Message object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneMathElementPerConstraint OneMathElementPerConstraint@endlink</td>
<td class="meaning">A Constraint object must contain one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneMessageElementPerConstraint OneMessageElementPerConstraint@endlink</td>
<td class="meaning">A Constraint object must contain one Message subobject</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnConstraint AllowedAttributesOnConstraint@endlink</td>
<td class="meaning">Invalid attribute found on Constraint object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoReactantsOrProducts NoReactantsOrProducts@endlink</td>
<td class="meaning">Cannot have a reaction with neither reactants nor products</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#IncorrectOrderInReaction IncorrectOrderInReaction@endlink</td>
<td class="meaning">Subobjects inside the Reaction object are not in the prescribed order</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#EmptyListInReaction EmptyListInReaction@endlink</td>
<td class="meaning">Reaction components, if present, cannot be empty</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidReactantsProductsList InvalidReactantsProductsList@endlink</td>
<td class="meaning">Invalid object found in the list of reactants or products</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidModifiersList InvalidModifiersList@endlink</td>
<td class="meaning">Invalid object found in the list of modifiers</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneSubElementPerReaction OneSubElementPerReaction@endlink</td>
<td class="meaning">A Reaction object can only contain one of each allowed type of object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompartmentOnReaction CompartmentOnReaction@endlink</td>
<td class="meaning">Invalid value for the Reaction 'compartment' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnReaction AllowedAttributesOnReaction@endlink</td>
<td class="meaning">Invalid attribute for a Reaction object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidSpeciesReference InvalidSpeciesReference@endlink</td>
<td class="meaning">Invalid 'species' attribute value in SpeciesReference object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BothStoichiometryAndMath BothStoichiometryAndMath@endlink</td>
<td class="meaning">The 'stoichiometry' attribute and StoichiometryMath subobject are mutually exclusive</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnSpeciesReference AllowedAttributesOnSpeciesReference@endlink</td>
<td class="meaning">Invalid attribute found on the SpeciesReference object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnModifier AllowedAttributesOnModifier@endlink</td>
<td class="meaning">Invalid attribute found on the ModifierSpeciesReference object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UndeclaredSpeciesRef UndeclaredSpeciesRef@endlink</td>
<td class="meaning">Unknown species referenced in the kinetic law <code>&lt;math&gt;</code> formula</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#IncorrectOrderInKineticLaw IncorrectOrderInKineticLaw@endlink</td>
<td class="meaning">Incorrect ordering of components in the KineticLaw object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#EmptyListInKineticLaw EmptyListInKineticLaw@endlink</td>
<td class="meaning">The list of parameters, if present, cannot be empty</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NonConstantLocalParameter NonConstantLocalParameter@endlink</td>
<td class="meaning">Parameters local to a KineticLaw object must have a 'constant' attribute value of 'true'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SubsUnitsNoLongerValid SubsUnitsNoLongerValid@endlink</td>
<td class="meaning">Attribute 'substanceUnits' is not supported in this Level+Version of SBML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#TimeUnitsNoLongerValid TimeUnitsNoLongerValid@endlink</td>
<td class="meaning">Attribute 'timeUnits' is not supported in this Level+Version of SBML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneListOfPerKineticLaw OneListOfPerKineticLaw@endlink</td>
<td class="meaning">Only one ListOfLocalParameters object is permitted within a KineticLaw object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyLocalParamsInListOfLocalParams OnlyLocalParamsInListOfLocalParams@endlink</td>
<td class="meaning">Only LocalParameter, Notes and Annotation objects are allowed in ListOfLocalParameter objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfLocalParam AllowedAttributesOnListOfLocalParam@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfLocalParameters object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneMathPerKineticLaw OneMathPerKineticLaw@endlink</td>
<td class="meaning">Only one <code>&lt;math&gt;</code> element is allowed in a KineticLaw object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UndeclaredSpeciesInStoichMath UndeclaredSpeciesInStoichMath@endlink</td>
<td class="meaning">Unknown species referenced in the StoichiometryMath object's <code>&lt;math&gt;</code> formula</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnKineticLaw AllowedAttributesOnKineticLaw@endlink</td>
<td class="meaning">Invalid attribute found on the KineticLaw object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfSpeciesRef AllowedAttributesOnListOfSpeciesRef@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfSpeciesReferences object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfMods AllowedAttributesOnListOfMods@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfModifiers object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnLocalParameter AllowedAttributesOnLocalParameter@endlink</td>
<td class="meaning">Invalid attribute found on the LocalParameter object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MissingTriggerInEvent MissingTriggerInEvent@endlink</td>
<td class="meaning">The Event object is missing a Trigger subobject</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#TriggerMathNotBoolean TriggerMathNotBoolean@endlink</td>
<td class="meaning">A Trigger object's <code>&lt;math&gt;</code> expression must evaluate to a Boolean value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MissingEventAssignment MissingEventAssignment@endlink</td>
<td class="meaning">The Event object is missing an EventAssignment subobject</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#TimeUnitsEvent TimeUnitsEvent@endlink</td>
<td class="meaning">Units referenced by 'timeUnits' attribute are not compatible with units of time</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#IncorrectOrderInEvent IncorrectOrderInEvent@endlink</td>
<td class="meaning">Incorrect ordering of components in Event object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ValuesFromTriggerTimeNeedDelay ValuesFromTriggerTimeNeedDelay@endlink</td>
<td class="meaning">Attribute 'useValuesFromTriggerTime'='false', but the Event object does not define a delay</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DelayNeedsValuesFromTriggerTime DelayNeedsValuesFromTriggerTime@endlink</td>
<td class="meaning">The use of a Delay object requires the Event attribute 'useValuesFromTriggerTime'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneMathPerTrigger OneMathPerTrigger@endlink</td>
<td class="meaning">A Trigger object must have one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneMathPerDelay OneMathPerDelay@endlink</td>
<td class="meaning">A Delay object must have one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidEventAssignmentVariable InvalidEventAssignmentVariable@endlink</td>
<td class="meaning">Invalid 'variable' attribute value in Event object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#EventAssignmentForConstantEntity EventAssignmentForConstantEntity@endlink</td>
<td class="meaning">An EventAssignment object cannot assign to a component having attribute 'constant'='true'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneMathPerEventAssignment OneMathPerEventAssignment@endlink</td>
<td class="meaning">An EventAssignment object must have one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnEventAssignment AllowedAttributesOnEventAssignment@endlink</td>
<td class="meaning">Invalid attribute found on the EventAssignment object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyOneDelayPerEvent OnlyOneDelayPerEvent@endlink</td>
<td class="meaning">An Event object can only have one Delay subobject</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneListOfEventAssignmentsPerEvent OneListOfEventAssignmentsPerEvent@endlink</td>
<td class="meaning">An Event object can only have one ListOfEventAssignments subobject</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyEventAssignInListOfEventAssign OnlyEventAssignInListOfEventAssign@endlink</td>
<td class="meaning">Only EventAssignment, Notes and Annotation objects are allowed in ListOfEventAssignments</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnListOfEventAssign AllowedAttributesOnListOfEventAssign@endlink</td>
<td class="meaning">Invalid attribute found on the ListOfEventAssignments object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnEvent AllowedAttributesOnEvent@endlink</td>
<td class="meaning">Invalid attribute found on the Event object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnTrigger AllowedAttributesOnTrigger@endlink</td>
<td class="meaning">Invalid attribute found on the Trigger object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnDelay AllowedAttributesOnDelay@endlink</td>
<td class="meaning">Invalid attribute found on the Delay object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#PersistentNotBoolean PersistentNotBoolean@endlink</td>
<td class="meaning">The Trigger attribute 'persistent' must evaluate to a Boolean value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InitialValueNotBoolean InitialValueNotBoolean@endlink</td>
<td class="meaning">The Trigger attribute 'initialValue' must evaluate to a Boolean value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OnlyOnePriorityPerEvent OnlyOnePriorityPerEvent@endlink</td>
<td class="meaning">An Event object can only have one Priority subobject</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OneMathPerPriority OneMathPerPriority@endlink</td>
<td class="meaning">A Priority object must have one <code>&lt;math&gt;</code> element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AllowedAttributesOnPriority AllowedAttributesOnPriority@endlink</td>
<td class="meaning">Invalid attribute found on the Priority object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompartmentShouldHaveSize CompartmentShouldHaveSize@endlink</td>
<td class="meaning">It's best to define a size for every compartment in a model</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SpeciesShouldHaveValue SpeciesShouldHaveValue@endlink</td>
<td class="meaning">It's best to define an initial amount or initial concentration for every species in a model</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ParameterShouldHaveUnits ParameterShouldHaveUnits@endlink</td>
<td class="meaning">It's best to declare units for every parameter in a model</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LocalParameterShadowsId LocalParameterShadowsId@endlink</td>
<td class="meaning">Local parameters defined within a kinetic law shadow global object symbols</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CannotConvertToL1V1 CannotConvertToL1V1@endlink</td>
<td class="meaning">Cannot convert to SBML Level 1 Version 1</td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoEventsInL1 NoEventsInL1@endlink</td>
<td class="meaning">SBML Level 1 does not support events</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoFunctionDefinitionsInL1 NoFunctionDefinitionsInL1@endlink</td>
<td class="meaning">SBML Level 1 does not support function definitions</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoConstraintsInL1 NoConstraintsInL1@endlink</td>
<td class="meaning">SBML Level 1 does not support constraints</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoInitialAssignmentsInL1 NoInitialAssignmentsInL1@endlink</td>
<td class="meaning">SBML Level 1 does not support initial assignments</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoSpeciesTypesInL1 NoSpeciesTypesInL1@endlink</td>
<td class="meaning">SBML Level 1 does not support species types</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoCompartmentTypeInL1 NoCompartmentTypeInL1@endlink</td>
<td class="meaning">SBML Level 1 does not support compartment types</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoNon3DCompartmentsInL1 NoNon3DCompartmentsInL1@endlink</td>
<td class="meaning">SBML Level 1 only supports three-dimensional compartments</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoFancyStoichiometryMathInL1 NoFancyStoichiometryMathInL1@endlink</td>
<td class="meaning">SBML Level 1 does not support non-integer nor non-rational stoichiometry formulas</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoNonIntegerStoichiometryInL1 NoNonIntegerStoichiometryInL1@endlink</td>
<td class="meaning">SBML Level 1 does not support non-integer 'stoichiometry' attribute values</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoUnitMultipliersOrOffsetsInL1 NoUnitMultipliersOrOffsetsInL1@endlink</td>
<td class="meaning">SBML Level 1 does not support multipliers or offsets in unit definitions</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SpeciesCompartmentRequiredInL1 SpeciesCompartmentRequiredInL1@endlink</td>
<td class="meaning">In SBML Level 1, a value for 'compartment' is mandatory in species definitions</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoSpeciesSpatialSizeUnitsInL1 NoSpeciesSpatialSizeUnitsInL1@endlink</td>
<td class="meaning">SBML Level 1 does not support species 'spatialSizeUnits' settings</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoSBOTermsInL1 NoSBOTermsInL1@endlink</td>
<td class="meaning">SBML Level 1 does not support the 'sboTerm' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#StrictUnitsRequiredInL1 StrictUnitsRequiredInL1@endlink</td>
<td class="meaning">SBML Level 1 requires strict unit consistency</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ConversionFactorNotInL1 ConversionFactorNotInL1@endlink</td>
<td class="meaning">SBML Level 1 does not support the 'conversionFactor' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompartmentNotOnL1Reaction CompartmentNotOnL1Reaction@endlink</td>
<td class="meaning">SBML Level 1 does not support the 'compartment' attribute on Reaction objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ExtentUnitsNotSubstance ExtentUnitsNotSubstance@endlink</td>
<td class="meaning">Units of extent must be compatible with units of substance</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#GlobalUnitsNotDeclared GlobalUnitsNotDeclared@endlink</td>
<td class="meaning">Global units must be refer to unit kind or unitDefinition.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#HasOnlySubstanceUnitsNotinL1 HasOnlySubstanceUnitsNotinL1@endlink</td>
<td class="meaning">The concept of hasOnlySubstanceUnits was not available in SBML Level 1.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AvogadroNotSupported AvogadroNotSupported@endlink</td>
<td class="meaning">Avogadro not supported in Levels 2 and 1.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoConstraintsInL2v1 NoConstraintsInL2v1@endlink</td>
<td class="meaning">SBML Level 2 Version 1 does not support Constraint objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoInitialAssignmentsInL2v1 NoInitialAssignmentsInL2v1@endlink</td>
<td class="meaning">SBML Level 2 Version 1 does not support InitialAssignment objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoSpeciesTypeInL2v1 NoSpeciesTypeInL2v1@endlink</td>
<td class="meaning">SBML Level 2 Version 1 does not support SpeciesType objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoCompartmentTypeInL2v1 NoCompartmentTypeInL2v1@endlink</td>
<td class="meaning">SBML Level 2 Version 1 does not support CompartmentType objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoSBOTermsInL2v1 NoSBOTermsInL2v1@endlink</td>
<td class="meaning">SBML Level 2 Version 1 does not support the 'sboTerm' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoIdOnSpeciesReferenceInL2v1 NoIdOnSpeciesReferenceInL2v1@endlink</td>
<td class="meaning">SBML Level 2 Version 1 does not support the 'id' attribute on SpeciesReference objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoDelayedEventAssignmentInL2v1 NoDelayedEventAssignmentInL2v1@endlink</td>
<td class="meaning">SBML Level 2 Version 1 does not support the 'useValuesFromTriggerTime' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#StrictUnitsRequiredInL2v1 StrictUnitsRequiredInL2v1@endlink</td>
<td class="meaning">SBML Level 2 Version 1 requires strict unit consistency</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#IntegerSpatialDimensions IntegerSpatialDimensions@endlink</td>
<td class="meaning">SBML Level 2 Version 1 requires that compartments have spatial dimensions of 0-3</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#StoichiometryMathNotYetSupported StoichiometryMathNotYetSupported@endlink</td>
<td class="meaning">Conversion to StoichiometryMath objects not yet supported</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#PriorityLostFromL3 PriorityLostFromL3@endlink</td>
<td class="meaning">SBML Level 2 Version 1 does not support priorities on Event objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NonPersistentNotSupported NonPersistentNotSupported@endlink</td>
<td class="meaning">SBML Level 2 Version 1 does not support the 'persistent' attribute on Trigger objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InitialValueFalseEventNotSupported InitialValueFalseEventNotSupported@endlink</td>
<td class="meaning">SBML Level 2 Version 1 does not support the 'initialValue' attribute on Trigger objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SBOTermNotUniversalInL2v2 SBOTermNotUniversalInL2v2@endlink</td>
<td class="meaning">The 'sboTerm' attribute is invalid for this component in SBML Level 2 Version 2</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoUnitOffsetInL2v2 NoUnitOffsetInL2v2@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'offset' attribute on Unit objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoKineticLawTimeUnitsInL2v2 NoKineticLawTimeUnitsInL2v2@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoKineticLawSubstanceUnitsInL2v2 NoKineticLawSubstanceUnitsInL2v2@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'substanceUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoDelayedEventAssignmentInL2v2 NoDelayedEventAssignmentInL2v2@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'useValuesFromTriggerTime' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ModelSBOBranchChangedBeyondL2v2 ModelSBOBranchChangedBeyondL2v2@endlink</td>
<td class="meaning">The allowable 'sboTerm' attribute values for Model objects differ for this SBML Level+Version</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#StrictUnitsRequiredInL2v2 StrictUnitsRequiredInL2v2@endlink</td>
<td class="meaning">SBML Level 2 Version 2 requires strict unit consistency</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#StrictSBORequiredInL2v2 StrictSBORequiredInL2v2@endlink</td>
<td class="meaning">SBML Level 2 Version 2 requires strict SBO term consistency</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DuplicateAnnotationInvalidInL2v2 DuplicateAnnotationInvalidInL2v2@endlink</td>
<td class="meaning">Duplicate top-level annotations are invalid in SBML Level 2 Version 2</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoUnitOffsetInL2v3 NoUnitOffsetInL2v3@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'offset' attribute on Unit objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoKineticLawTimeUnitsInL2v3 NoKineticLawTimeUnitsInL2v3@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoKineticLawSubstanceUnitsInL2v3 NoKineticLawSubstanceUnitsInL2v3@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'substanceUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoSpeciesSpatialSizeUnitsInL2v3 NoSpeciesSpatialSizeUnitsInL2v3@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'spatialSizeUnit' attribute on Species objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoEventTimeUnitsInL2v3 NoEventTimeUnitsInL2v3@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on Event objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoDelayedEventAssignmentInL2v3 NoDelayedEventAssignmentInL2v3@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'useValuesFromTriggerTime' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ModelSBOBranchChangedBeyondL2v3 ModelSBOBranchChangedBeyondL2v3@endlink</td>
<td class="meaning">The allowable 'sboTerm' attribute values for Model objects differ for this SBML Level+Version</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#StrictUnitsRequiredInL2v3 StrictUnitsRequiredInL2v3@endlink</td>
<td class="meaning">SBML Level 2 Version 3 requires strict unit consistency</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#StrictSBORequiredInL2v3 StrictSBORequiredInL2v3@endlink</td>
<td class="meaning">SBML Level 2 Version 3 requires strict SBO term consistency</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DuplicateAnnotationInvalidInL2v3 DuplicateAnnotationInvalidInL2v3@endlink</td>
<td class="meaning">Duplicate top-level annotations are invalid in SBML Level 2 Version 3</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoUnitOffsetInL2v4 NoUnitOffsetInL2v4@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'offset' attribute on Unit objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoKineticLawTimeUnitsInL2v4 NoKineticLawTimeUnitsInL2v4@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoKineticLawSubstanceUnitsInL2v4 NoKineticLawSubstanceUnitsInL2v4@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'substanceUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoSpeciesSpatialSizeUnitsInL2v4 NoSpeciesSpatialSizeUnitsInL2v4@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'spatialSizeUnit' attribute on Species objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoEventTimeUnitsInL2v4 NoEventTimeUnitsInL2v4@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on Event objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ModelSBOBranchChangedInL2v4 ModelSBOBranchChangedInL2v4@endlink</td>
<td class="meaning">The allowable 'sboTerm' attribute values for Model objects differ for this SBML Level+Version</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DuplicateAnnotationInvalidInL2v4 DuplicateAnnotationInvalidInL2v4@endlink</td>
<td class="meaning">Duplicate top-level annotations are invalid in SBML Level 2 Version 4</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoSpeciesTypeInL3v1 NoSpeciesTypeInL3v1@endlink</td>
<td class="meaning">SBML Level 3 Version 1 does not support SpeciesType objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoCompartmentTypeInL3v1 NoCompartmentTypeInL3v1@endlink</td>
<td class="meaning">SBML Level 3 Version 1 does not support CompartmentType objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoUnitOffsetInL3v1 NoUnitOffsetInL3v1@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'offset' attribute on Unit objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoKineticLawTimeUnitsInL3v1 NoKineticLawTimeUnitsInL3v1@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoKineticLawSubstanceUnitsInL3v1 NoKineticLawSubstanceUnitsInL3v1@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'substanceUnits' attribute on KineticLaw objects</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoSpeciesSpatialSizeUnitsInL3v1 NoSpeciesSpatialSizeUnitsInL3v1@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'spatialSizeUnit' attribute on Species objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoEventTimeUnitsInL3v1 NoEventTimeUnitsInL3v1@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on Event objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ModelSBOBranchChangedInL3v1 ModelSBOBranchChangedInL3v1@endlink</td>
<td class="meaning">The allowable 'sboTerm' attribute values for Model objects differ for this SBML Level+Version</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DuplicateAnnotationInvalidInL3v1 DuplicateAnnotationInvalidInL3v1@endlink</td>
<td class="meaning">Duplicate top-level annotations are invalid in SBML Level 3 Version 1</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoCompartmentOutsideInL3v1 NoCompartmentOutsideInL3v1@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'outside' attribute on Compartment objects</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoStoichiometryMathInL3v1 NoStoichiometryMathInL3v1@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the StoichiometryMath object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidSBMLLevelVersion InvalidSBMLLevelVersion@endlink</td>
<td class="meaning">Unknown Level+Version combination of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AnnotationNotesNotAllowedLevel1 AnnotationNotesNotAllowedLevel1@endlink</td>
<td class="meaning">Annotation objects on the SBML container element are not permitted in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidRuleOrdering InvalidRuleOrdering@endlink</td>
<td class="meaning">Invalid ordering of rules</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#RequiredPackagePresent RequiredPackagePresent@endlink</td>
<td class="meaning">The SBML document requires an SBML Level 3 package unavailable in this software</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UnrequiredPackagePresent UnrequiredPackagePresent@endlink</td>
<td class="meaning">The SBML document uses an SBML Level 3 package unavailable in this software</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#PackageRequiredShouldBeFalse PackageRequiredShouldBeFalse@endlink</td>
<td class="meaning">This package expects required to be false</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SubsUnitsAllowedInKL SubsUnitsAllowedInKL@endlink</td>
<td class="meaning">Disallowed value for attribute 'substanceUnits' on KineticLaw object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#TimeUnitsAllowedInKL TimeUnitsAllowedInKL@endlink</td>
<td class="meaning">Disallowed value for attribute 'timeUnits' on KineticLaw object</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FormulaInLevel1KL FormulaInLevel1KL@endlink</td>
<td class="meaning">Only predefined functions are allowed in SBML Level 1 formulas</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#L3SubstanceUnitsOnModel L3SubstanceUnitsOnModel@endlink</td>
<td class="meaning">Invalid 'substanceUnits' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#TimeUnitsRemoved TimeUnitsRemoved@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'timeUnits' attribute on Event objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadMathML BadMathML@endlink</td>
<td class="meaning">Invalid MathML expression</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FailedMathMLReadOfDouble FailedMathMLReadOfDouble@endlink</td>
<td class="meaning">Missing or invalid floating-point number in MathML expression</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FailedMathMLReadOfInteger FailedMathMLReadOfInteger@endlink</td>
<td class="meaning">Missing or invalid integer in MathML expression</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FailedMathMLReadOfExponential FailedMathMLReadOfExponential@endlink</td>
<td class="meaning">Missing or invalid exponential expression in MathML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FailedMathMLReadOfRational FailedMathMLReadOfRational@endlink</td>
<td class="meaning">Missing or invalid rational expression in MathML</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#BadMathMLNodeType BadMathMLNodeType@endlink</td>
<td class="meaning">Invalid MathML element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidMathMLAttribute InvalidMathMLAttribute@endlink</td>
<td class="meaning">Invalid MathML attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoTimeSymbolInFunctionDef NoTimeSymbolInFunctionDef@endlink</td>
<td class="meaning">Use of <code>&lt;csymbol&gt;</code> for 'time' not allowed within FunctionDefinition objects</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NoBodyInFunctionDef NoBodyInFunctionDef@endlink</td>
<td class="meaning">There must be a <code>&lt;lambda&gt;</code> body within the <code>&lt;math&gt;</code> element of a FunctionDefinition object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#DanglingUnitSIdRef DanglingUnitSIdRef@endlink</td>
<td class="meaning">Units must refer to valid unit or unitDefinition</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#RDFMissingAboutTag RDFMissingAboutTag@endlink</td>
<td class="meaning">RDF missing the <code>&lt;about&gt;</code> tag</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#RDFEmptyAboutTag RDFEmptyAboutTag@endlink</td>
<td class="meaning">RDF empty <code>&lt;about&gt;</code> tag</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#RDFAboutTagNotMetaid RDFAboutTagNotMetaid@endlink</td>
<td class="meaning">RDF <code>&lt;about&gt;</code> tag is not metaid</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#RDFNotCompleteModelHistory RDFNotCompleteModelHistory@endlink</td>
<td class="meaning">RDF does not contain valid ModelHistory</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#RDFNotModelHistory RDFNotModelHistory@endlink</td>
<td class="meaning">RDF does not result in a ModelHistory</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#AnnotationNotElement AnnotationNotElement@endlink</td>
<td class="meaning">Annotation must contain element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UndeclaredUnits UndeclaredUnits@endlink</td>
<td class="meaning">Missing unit declarations on parameters or literal numbers in expression</td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UndeclaredTimeUnitsL3 UndeclaredTimeUnitsL3@endlink</td>
<td class="meaning">Unable to verify consistency of units: the unit of time has not been declared</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UndeclaredExtentUnitsL3 UndeclaredExtentUnitsL3@endlink</td>
<td class="meaning">Unable to verify consistency of units: the units of reaction extent have not been declared</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UndeclaredObjectUnitsL3 UndeclaredObjectUnitsL3@endlink</td>
<td class="meaning">Unable to verify consistency of units: encountered a model entity with no declared units</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UnrecognisedSBOTerm UnrecognisedSBOTerm@endlink</td>
<td class="meaning">Unrecognized 'sboTerm' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ObseleteSBOTerm ObseleteSBOTerm@endlink</td>
<td class="meaning">Obsolete 'sboTerm' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#IncorrectCompartmentSpatialDimensions IncorrectCompartmentSpatialDimensions@endlink</td>
<td class="meaning">In SBML Level 1, only three-dimensional compartments are allowed</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompartmentTypeNotValidAttribute CompartmentTypeNotValidAttribute@endlink</td>
<td class="meaning">CompartmentType objects are not available in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ConstantNotValidAttribute ConstantNotValidAttribute@endlink</td>
<td class="meaning">This Level+Version of SBML does not support the 'constant' attribute on this component</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MetaIdNotValidAttribute MetaIdNotValidAttribute@endlink</td>
<td class="meaning">Attribute 'metaid' is not available in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SBOTermNotValidAttributeBeforeL2V3 SBOTermNotValidAttributeBeforeL2V3@endlink</td>
<td class="meaning">The 'sboTerm' attribute is not available on this component before SBML Level 2 Version 3</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidL1CompartmentUnits InvalidL1CompartmentUnits@endlink</td>
<td class="meaning">Invalid units for a compartment in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#L1V1CompartmentVolumeReqd L1V1CompartmentVolumeReqd@endlink</td>
<td class="meaning">In SBML Level 1, a compartment's volume must be specified</td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompartmentTypeNotValidComponent CompartmentTypeNotValidComponent@endlink</td>
<td class="meaning">CompartmentType objects are not available in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ConstraintNotValidComponent ConstraintNotValidComponent@endlink</td>
<td class="meaning">Constraint objects are not available in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#EventNotValidComponent EventNotValidComponent@endlink</td>
<td class="meaning">Event objects are not available in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SBOTermNotValidAttributeBeforeL2V2 SBOTermNotValidAttributeBeforeL2V2@endlink</td>
<td class="meaning">The 'sboTerm' attribute is invalid for this component before Level 2 Version 2</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FuncDefNotValidComponent FuncDefNotValidComponent@endlink</td>
<td class="meaning">FunctionDefinition objects are not available in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InitialAssignNotValidComponent InitialAssignNotValidComponent@endlink</td>
<td class="meaning">InitialAssignment objects are not available in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#VariableNotValidAttribute VariableNotValidAttribute@endlink</td>
<td class="meaning">Attribute 'variable' is not available on this component in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#UnitsNotValidAttribute UnitsNotValidAttribute@endlink</td>
<td class="meaning">Attribute 'units' is not available on this component in this Level+Version of SBML</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#ConstantSpeciesNotValidAttribute ConstantSpeciesNotValidAttribute@endlink</td>
<td class="meaning">Attribute 'constant' is not available on Species objects in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SpatialSizeUnitsNotValidAttribute SpatialSizeUnitsNotValidAttribute@endlink</td>
<td class="meaning">Attribute 'spatialSizeUnits' is not available on Species objects in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SpeciesTypeNotValidAttribute SpeciesTypeNotValidAttribute@endlink</td>
<td class="meaning">Attribute 'speciesType' is not available on Species objects in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#HasOnlySubsUnitsNotValidAttribute HasOnlySubsUnitsNotValidAttribute@endlink</td>
<td class="meaning">Attribute 'hasOnlySubstanceUnits' is not available on Species objects in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#IdNotValidAttribute IdNotValidAttribute@endlink</td>
<td class="meaning">Attribute 'id' is not available on SpeciesReference objects in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#NameNotValidAttribute NameNotValidAttribute@endlink</td>
<td class="meaning">Attribute 'name' is not available on SpeciesReference objects in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#SpeciesTypeNotValidComponent SpeciesTypeNotValidComponent@endlink</td>
<td class="meaning">The SpeciesType object is not supported in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#StoichiometryMathNotValidComponent StoichiometryMathNotValidComponent@endlink</td>
<td class="meaning">The StoichiometryMath object is not supported in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#MultiplierNotValidAttribute MultiplierNotValidAttribute@endlink</td>
<td class="meaning">Attribute 'multiplier' on Unit objects is not supported in SBML Level 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#OffsetNotValidAttribute OffsetNotValidAttribute@endlink</td>
<td class="meaning">Attribute 'offset' on Unit objects is only available in SBML Level 2 Version 1</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#L3SpatialDimensionsUnset L3SpatialDimensionsUnset@endlink</td>
<td class="meaning">No value given for 'spatialDimensions' attribute; assuming a value of 3</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#PackageConversionNotSupported PackageConversionNotSupported@endlink</td>
<td class="meaning">Conversion of SBML Level 3 package constructs is not yet supported</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#InvalidTargetLevelVersion InvalidTargetLevelVersion@endlink</td>
<td class="meaning">The requested SBML Level/Version combination is not known to exist</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#L3NotSupported L3NotSupported@endlink</td>
<td class="meaning">SBML Level 3 is not yet supported</td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompUnknown CompUnknown@endlink</td>
<td class="meaning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompNSUndeclared CompNSUndeclared@endlink</td>
<td class="meaning">The comp ns is not correctly declared</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompElementNotInNs CompElementNotInNs@endlink</td>
<td class="meaning">Element not in comp namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompDuplicateComponentId CompDuplicateComponentId@endlink</td>
<td class="meaning">Duplicate 'id' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompUniqueModelIds CompUniqueModelIds@endlink</td>
<td class="meaning">Model and ExternalModelDefinitions must have unique ids</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompUniquePortIds CompUniquePortIds@endlink</td>
<td class="meaning">Ports must have unique ids</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompInvalidSIdSyntax CompInvalidSIdSyntax@endlink</td>
<td class="meaning">Invalid SId syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompInvalidSubmodelRefSyntax CompInvalidSubmodelRefSyntax@endlink</td>
<td class="meaning">Invalid submodelRef syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompInvalidDeletionSyntax CompInvalidDeletionSyntax@endlink</td>
<td class="meaning">Invalid deletion syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompInvalidConversionFactorSyntax CompInvalidConversionFactorSyntax@endlink</td>
<td class="meaning">Invalid conversionFactor syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompInvalidNameSyntax CompInvalidNameSyntax@endlink</td>
<td class="meaning">Invalid name syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompReplacedUnitsShouldMatch CompReplacedUnitsShouldMatch@endlink</td>
<td class="meaning">Units of replaced elements should match replacement units.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompOneListOfReplacedElements CompOneListOfReplacedElements@endlink</td>
<td class="meaning">Only one <code>&lt;listOfReplacedElements&gt;</code> allowed.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompLOReplaceElementsAllowedElements CompLOReplaceElementsAllowedElements@endlink</td>
<td class="meaning">Allowed children of <code>&lt;listOfReplacedElements&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompLOReplacedElementsAllowedAttribs CompLOReplacedElementsAllowedAttribs@endlink</td>
<td class="meaning">Allowed <code>&lt;listOfReplacedElements&gt;</code> attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompEmptyLOReplacedElements CompEmptyLOReplacedElements@endlink</td>
<td class="meaning"><code>&lt;listOfReplacedElements&gt;</code> must not be empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompOneReplacedByElement CompOneReplacedByElement@endlink</td>
<td class="meaning">Only one <code>&lt;replacedBy&gt;</code> object allowed.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompAttributeRequiredMissing CompAttributeRequiredMissing@endlink</td>
<td class="meaning">Required comp:required attribute on <code>&lt;sbml&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompAttributeRequiredMustBeBoolean CompAttributeRequiredMustBeBoolean@endlink</td>
<td class="meaning">The comp:required attribute must be Boolean</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompRequiredTrueIfElementsRemain CompRequiredTrueIfElementsRemain@endlink</td>
<td class="meaning">The comp:required attribute must be 'true' if math changes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompRequiredFalseIfAllElementsReplaced CompRequiredFalseIfAllElementsReplaced@endlink</td>
<td class="meaning">The comp:required attribute must be 'false' if math does not change</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompOneListOfModelDefinitions CompOneListOfModelDefinitions@endlink</td>
<td class="meaning">Only one <code>&lt;listOfModelDefinitions&gt;</code> allowed.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompEmptyLOModelDefs CompEmptyLOModelDefs@endlink</td>
<td class="meaning"><code>&lt;listOfModelDefinitions&gt;</code> and <code>&lt;listOfExternalModelDefinitions&gt;</code> must not be empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompLOModelDefsAllowedElements CompLOModelDefsAllowedElements@endlink</td>
<td class="meaning">Only <code>&lt;modelDefinitions&gt;</code> in <code>&lt;listOfModelDefinitions&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompLOExtModelDefsAllowedElements CompLOExtModelDefsAllowedElements@endlink</td>
<td class="meaning">Only <code>&lt;externalModelDefinitions&gt;</code> in <code>&lt;listOfExternalModelDefinitions&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompLOModelDefsAllowedAttributes CompLOModelDefsAllowedAttributes@endlink</td>
<td class="meaning">Allowed <code>&lt;listOfModelDefinitions&gt;</code> attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompLOExtModDefsAllowedAttributes CompLOExtModDefsAllowedAttributes@endlink</td>
<td class="meaning">Allowed <code>&lt;listOfExternalModelDefinitions&gt;</code> attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompOneListOfExtModelDefinitions CompOneListOfExtModelDefinitions@endlink</td>
<td class="meaning">Only one <code>&lt;listOfExternalModelDefinitions&gt;</code> allowed.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompAttributeRequiredMustBeTrue CompAttributeRequiredMustBeTrue@endlink</td>
<td class="meaning">The comp:required attribute must be 'true'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompExtModDefAllowedCoreAttributes CompExtModDefAllowedCoreAttributes@endlink</td>
<td class="meaning">Allowed <code>&lt;externalModelDefinitions&gt;</code> core attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompExtModDefAllowedElements CompExtModDefAllowedElements@endlink</td>
<td class="meaning">Allowed <code>&lt;externalModelDefinitions&gt;</code> elements</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompExtModDefAllowedAttributes CompExtModDefAllowedAttributes@endlink</td>
<td class="meaning">Allowed <code>&lt;externalModelDefinitions&gt;</code> attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompReferenceMustBeL3 CompReferenceMustBeL3@endlink</td>
<td class="meaning">External models must be L3</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompModReferenceMustIdOfModel CompModReferenceMustIdOfModel@endlink</td>
<td class="meaning">'modelRef' must be the 'id' of a model in the 'source' document</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompExtModMd5DoesNotMatch CompExtModMd5DoesNotMatch@endlink</td>
<td class="meaning">MD5 checksum does not match the 'source' document</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompInvalidSourceSyntax CompInvalidSourceSyntax@endlink</td>
<td class="meaning">The 'comp:source' attribute must be of type 'anyURI'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompInvalidModelRefSyntax CompInvalidModelRefSyntax@endlink</td>
<td class="meaning">The 'comp:modelRef' attribute must have the syntax of 'SId'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompInvalidMD5Syntax CompInvalidMD5Syntax@endlink</td>
<td class="meaning">The 'comp:md5' attribute must have the syntax of 'string'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompCircularExternalModelReference CompCircularExternalModelReference@endlink</td>
<td class="meaning">Circular reference in <code>&lt;externalModelDefinition&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompOneListOfOnModel CompOneListOfOnModel@endlink</td>
<td class="meaning">Only one <code>&lt;listOfSubmodels&gt;</code> and one <code>&lt;listOfPorts&gt;</code> allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompNoEmptyListOfOnModel CompNoEmptyListOfOnModel@endlink</td>
<td class="meaning">No empty listOf elements allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompLOSubmodelsAllowedElements CompLOSubmodelsAllowedElements@endlink</td>
<td class="meaning">Allowed elements on <code>&lt;listOfSubmodels&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompLOPortsAllowedElements CompLOPortsAllowedElements@endlink</td>
<td class="meaning">Allowed elements on <code>&lt;listOfPorts&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompLOSubmodelsAllowedAttributes CompLOSubmodelsAllowedAttributes@endlink</td>
<td class="meaning">Allowed attributes on <code>&lt;listOfSubmodels&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompLOPortsAllowedAttributes CompLOPortsAllowedAttributes@endlink</td>
<td class="meaning">Allowed attributes on <code>&lt;listOfPorts&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompSubmodelAllowedCoreAttributes CompSubmodelAllowedCoreAttributes@endlink</td>
<td class="meaning">Allowed core attributes on <code>&lt;submodel&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompSubmodelAllowedElements CompSubmodelAllowedElements@endlink</td>
<td class="meaning">Allowed elements on <code>&lt;submodel&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompOneListOfDeletionOnSubmodel CompOneListOfDeletionOnSubmodel@endlink</td>
<td class="meaning">Only one <code>&lt;listOfDeletions&gt;</code> on a <code>&lt;submodel&gt;</code> allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompSubmodelNoEmptyLODeletions CompSubmodelNoEmptyLODeletions@endlink</td>
<td class="meaning">No empty listOfDeletions elements allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompLODeletionsAllowedElements CompLODeletionsAllowedElements@endlink</td>
<td class="meaning">Allowed elements on <code>&lt;listOfDeletions&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompLODeletionAllowedAttributes CompLODeletionAllowedAttributes@endlink</td>
<td class="meaning">Allowed <code>&lt;listOfDeletions&gt;</code> attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompSubmodelAllowedAttributes CompSubmodelAllowedAttributes@endlink</td>
<td class="meaning">Allowed <code>&lt;submodel&gt;</code> attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompModReferenceSyntax CompModReferenceSyntax@endlink</td>
<td class="meaning">'comp:modelRef' must conform to SId syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompInvalidTimeConvFactorSyntax CompInvalidTimeConvFactorSyntax@endlink</td>
<td class="meaning">'comp:timeConversionFactor' must conform to SId syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompInvalidExtentConvFactorSyntax CompInvalidExtentConvFactorSyntax@endlink</td>
<td class="meaning">'comp:extentConversionFactor' must conform to SId syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompSubmodelMustReferenceModel CompSubmodelMustReferenceModel@endlink</td>
<td class="meaning">The 'comp:modelRef' attribute must reference a model</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompSubmodelCannotReferenceSelf CompSubmodelCannotReferenceSelf@endlink</td>
<td class="meaning">The 'comp:modelRef' attribute cannot reference own model</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompModCannotCircularlyReferenceSelf CompModCannotCircularlyReferenceSelf@endlink</td>
<td class="meaning"><code>&lt;model&gt;</code> may not reference <code>&lt;submodel&gt;</code> that references itself.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompTimeConversionMustBeParameter CompTimeConversionMustBeParameter@endlink</td>
<td class="meaning">The 'comp:timeConversionFactor' must reference a parameter</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompExtentConversionMustBeParameter CompExtentConversionMustBeParameter@endlink</td>
<td class="meaning">The 'comp:extentConversionFactor' must reference a parameter</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompPortRefMustReferencePort CompPortRefMustReferencePort@endlink</td>
<td class="meaning">The 'comp:portRef' attribute must be the 'id' of a <code>&lt;port&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompIdRefMustReferenceObject CompIdRefMustReferenceObject@endlink</td>
<td class="meaning">The 'comp:idRef' attribute must be the 'id' of a model element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompUnitRefMustReferenceUnitDef CompUnitRefMustReferenceUnitDef@endlink</td>
<td class="meaning">The 'comp:unitRef' attribute must be the 'id' of a UnitDefinition</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompMetaIdRefMustReferenceObject CompMetaIdRefMustReferenceObject@endlink</td>
<td class="meaning">The 'comp:metaIdRef' attribute must be the 'metaid' of an object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompParentOfSBRefChildMustBeSubmodel CompParentOfSBRefChildMustBeSubmodel@endlink</td>
<td class="meaning">If <code>&lt;sBaseRef&gt;</code> has a child <code>&lt;sBaseRef&gt;</code> its parent must be a <code>&lt;submodel&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompInvalidPortRefSyntax CompInvalidPortRefSyntax@endlink</td>
<td class="meaning">The 'comp:portRef' attribute must have the syntax of an SBML SId</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompInvalidIdRefSyntax CompInvalidIdRefSyntax@endlink</td>
<td class="meaning">The 'comp:idRef' attribute must have the syntax of an SBML SId</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompInvalidUnitRefSyntax CompInvalidUnitRefSyntax@endlink</td>
<td class="meaning">The 'comp:unitRef' attribute must have the syntax of an SBML SId</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompInvalidMetaIdRefSyntax CompInvalidMetaIdRefSyntax@endlink</td>
<td class="meaning">The 'comp:metaIdRef' attribute must have the syntax of an XML ID</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompOneSBaseRefOnly CompOneSBaseRefOnly@endlink</td>
<td class="meaning">Only one <code>&lt;sbaseRef&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompDeprecatedSBaseRefSpelling CompDeprecatedSBaseRefSpelling@endlink</td>
<td class="meaning">The spelling 'sbaseRef' is deprecated</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompSBaseRefMustReferenceObject CompSBaseRefMustReferenceObject@endlink</td>
<td class="meaning">An SBaseRef must reference an object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompSBaseRefMustReferenceOnlyOneObject CompSBaseRefMustReferenceOnlyOneObject@endlink</td>
<td class="meaning">An SBaseRef must reference only one other object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompNoMultipleReferences CompNoMultipleReferences@endlink</td>
<td class="meaning">Objects may not be referenced by mutiple SBaseRef constructs.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompPortMustReferenceObject CompPortMustReferenceObject@endlink</td>
<td class="meaning">Port must reference an object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompPortMustReferenceOnlyOneObject CompPortMustReferenceOnlyOneObject@endlink</td>
<td class="meaning">Port must reference only one other object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompPortAllowedAttributes CompPortAllowedAttributes@endlink</td>
<td class="meaning">Allowed attributes on a Port</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompPortReferencesUnique CompPortReferencesUnique@endlink</td>
<td class="meaning">Port definitions must be unique.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompDeletionMustReferenceObject CompDeletionMustReferenceObject@endlink</td>
<td class="meaning">Deletion must reference an object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompDeletionMustReferOnlyOneObject CompDeletionMustReferOnlyOneObject@endlink</td>
<td class="meaning">Deletion must reference only one other object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompDeletionAllowedAttributes CompDeletionAllowedAttributes@endlink</td>
<td class="meaning">Allowed attributes on a Deletion</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompReplacedElementMustRefObject CompReplacedElementMustRefObject@endlink</td>
<td class="meaning">ReplacedElement must reference an object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompReplacedElementMustRefOnlyOne CompReplacedElementMustRefOnlyOne@endlink</td>
<td class="meaning">ReplacedElement must reference only one other object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompReplacedElementAllowedAttributes CompReplacedElementAllowedAttributes@endlink</td>
<td class="meaning">Allowed attributes on <code>&lt;replacedElement&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompReplacedElementSubModelRef CompReplacedElementSubModelRef@endlink</td>
<td class="meaning">The 'comp:submodelRef' attribute must point to a <code>&lt;submodel&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompReplacedElementDeletionRef CompReplacedElementDeletionRef@endlink</td>
<td class="meaning">The 'comp:deletion' attribute must point to a <code>&lt;deletion&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompReplacedElementConvFactorRef CompReplacedElementConvFactorRef@endlink</td>
<td class="meaning">The 'comp:conversionFactor attribute must point to a <code>&lt;parameter&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompReplacedElementSameReference CompReplacedElementSameReference@endlink</td>
<td class="meaning">No <code>&lt;replacedElement&gt;</code> refer to same object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompReplacedElementNoDelAndConvFact CompReplacedElementNoDelAndConvFact@endlink</td>
<td class="meaning">No <code>&lt;replacedElement&gt;</code> with deletion and conversionfactor</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompReplacedByMustRefObject CompReplacedByMustRefObject@endlink</td>
<td class="meaning">ReplacedBy must reference an object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompReplacedByMustRefOnlyOne CompReplacedByMustRefOnlyOne@endlink</td>
<td class="meaning">ReplacedBy must reference only one other object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompReplacedByAllowedAttributes CompReplacedByAllowedAttributes@endlink</td>
<td class="meaning">Allowed attributes on <code>&lt;replacedBy&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompReplacedBySubModelRef CompReplacedBySubModelRef@endlink</td>
<td class="meaning">The 'comp:submodelRef' attribute must point to a <code>&lt;submodel&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompMustReplaceSameClass CompMustReplaceSameClass@endlink</td>
<td class="meaning">Replaced classes must match.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompMustReplaceIDs CompMustReplaceIDs@endlink</td>
<td class="meaning">Replaced IDs must be replaced with IDs.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompMustReplaceMetaIDs CompMustReplaceMetaIDs@endlink</td>
<td class="meaning">Replaced metaids must be replaced with metaids.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompMustReplacePackageIDs CompMustReplacePackageIDs@endlink</td>
<td class="meaning">Replaced package IDs must be replaced with package IDs.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompUnresolvedReference CompUnresolvedReference@endlink</td>
<td class="meaning">Unresolved reference.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompNoModelInReference CompNoModelInReference@endlink</td>
<td class="meaning">No model in referenced document.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompExtModDefBad CompExtModDefBad@endlink</td>
<td class="meaning">Referenced <code>&lt;externalModelDefinition&gt;</code> unresolvable.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompModelFlatteningFailed CompModelFlatteningFailed@endlink</td>
<td class="meaning">Model failed to flatten.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompFlatModelNotValid CompFlatModelNotValid@endlink</td>
<td class="meaning">Flat model not valid.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompLineNumbersUnreliable CompLineNumbersUnreliable@endlink</td>
<td class="meaning">Line numbers unreliable.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompFlatteningNotRecognisedReqd CompFlatteningNotRecognisedReqd@endlink</td>
<td class="meaning">Flattening not implemented for required package.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompFlatteningNotRecognisedNotReqd CompFlatteningNotRecognisedNotReqd@endlink</td>
<td class="meaning">Flattening not implemented for unrequired package.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompFlatteningNotImplementedNotReqd CompFlatteningNotImplementedNotReqd@endlink</td>
<td class="meaning">Flattening not implemented for unrequired package.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompFlatteningNotImplementedReqd CompFlatteningNotImplementedReqd@endlink</td>
<td class="meaning">Flattening not implemented for required package.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompFlatteningWarning CompFlatteningWarning@endlink</td>
<td class="meaning">Flattening reference may come from package.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompDeprecatedDeleteFunction CompDeprecatedDeleteFunction@endlink</td>
<td class="meaning">The performDeletions functions is deprecated.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompDeprecatedReplaceFunction CompDeprecatedReplaceFunction@endlink</td>
<td class="meaning">The performReplacementsAndConversions fuctions is deprecated.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompDeletedReplacement CompDeletedReplacement@endlink</td>
<td class="meaning">Element deleted before a subelement could be replaced.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompIdRefMayReferenceUnknownPackage CompIdRefMayReferenceUnknownPackage@endlink</td>
<td class="meaning">The 'comp:idRef' attribute must be the 'id' of a model element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#CompMetaIdRefMayReferenceUnknownPkg CompMetaIdRefMayReferenceUnknownPkg@endlink</td>
<td class="meaning">The 'comp:metaIdRef' attribute must be the 'metaid' of a model element</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcUnknown FbcUnknown@endlink</td>
<td class="meaning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcNSUndeclared FbcNSUndeclared@endlink</td>
<td class="meaning">The fbc ns is not correctly declared</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcElementNotInNs FbcElementNotInNs@endlink</td>
<td class="meaning">Element not in fbc namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcDuplicateComponentId FbcDuplicateComponentId@endlink</td>
<td class="meaning">Duplicate 'id' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcSBMLSIdSyntax FbcSBMLSIdSyntax@endlink</td>
<td class="meaning">Invalid 'id' attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcAttributeRequiredMissing FbcAttributeRequiredMissing@endlink</td>
<td class="meaning">Required fbc:required attribute on <code>&lt;sbml&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcAttributeRequiredMustBeBoolean FbcAttributeRequiredMustBeBoolean@endlink</td>
<td class="meaning">The fbc:required attribute must be Boolean</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcRequiredFalse FbcRequiredFalse@endlink</td>
<td class="meaning">The fbc:required attribute must be 'false'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcOnlyOneEachListOf FbcOnlyOneEachListOf@endlink</td>
<td class="meaning">One of each list of allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcNoEmptyListOfs FbcNoEmptyListOfs@endlink</td>
<td class="meaning">ListOf elements cannot be empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcLOFluxBoundsAllowedElements FbcLOFluxBoundsAllowedElements@endlink</td>
<td class="meaning">Allowed elements on ListOfFluxBounds</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcLOObjectivesAllowedElements FbcLOObjectivesAllowedElements@endlink</td>
<td class="meaning">Allowed elements on ListOfObjectives</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcLOFluxBoundsAllowedAttributes FbcLOFluxBoundsAllowedAttributes@endlink</td>
<td class="meaning">Allowed attributes on ListOfFluxBounds</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcLOObjectivesAllowedAttributes FbcLOObjectivesAllowedAttributes@endlink</td>
<td class="meaning">Allowed attributes on ListOfObjectives</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcActiveObjectiveSyntax FbcActiveObjectiveSyntax@endlink</td>
<td class="meaning">Type of activeObjective attribute</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcActiveObjectiveRefersObjective FbcActiveObjectiveRefersObjective@endlink</td>
<td class="meaning">ActiveObjective must reference Objective</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcSpeciesAllowedL3Attributes FbcSpeciesAllowedL3Attributes@endlink</td>
<td class="meaning">Species allowed attributes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcSpeciesChargeMustBeInteger FbcSpeciesChargeMustBeInteger@endlink</td>
<td class="meaning">Charge must be integer</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcSpeciesFormulaMustBeString FbcSpeciesFormulaMustBeString@endlink</td>
<td class="meaning">Chemical formula must be string</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxBoundAllowedL3Attributes FbcFluxBoundAllowedL3Attributes@endlink</td>
<td class="meaning"><code>&lt;fluxBound&gt;</code> may only have 'metaId' and 'sboTerm' from L3 namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxBoundAllowedElements FbcFluxBoundAllowedElements@endlink</td>
<td class="meaning"><code>&lt;fluxBound&gt;</code> may only have <code>&lt;notes&gt;</code> and <code>&lt;annotations&gt;</code> from L3 Core</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxBoundRequiredAttributes FbcFluxBoundRequiredAttributes@endlink</td>
<td class="meaning">Invalid attribute found on <code>&lt;fluxBound&gt;</code> object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxBoundRectionMustBeSIdRef FbcFluxBoundRectionMustBeSIdRef@endlink</td>
<td class="meaning">Datatype for 'fbc:reaction' must be SIdRef</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxBoundNameMustBeString FbcFluxBoundNameMustBeString@endlink</td>
<td class="meaning">The attribute 'fbc:name' must be of the data type string</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxBoundOperationMustBeEnum FbcFluxBoundOperationMustBeEnum@endlink</td>
<td class="meaning">The attribute 'fbc:operation' must be of data type FbcOperation</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxBoundValueMustBeDouble FbcFluxBoundValueMustBeDouble@endlink</td>
<td class="meaning">The attribute 'fbc:value' must be of the data type double</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxBoundReactionMustExist FbcFluxBoundReactionMustExist@endlink</td>
<td class="meaning">'fbc:reaction' must refer to valid reaction</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxBoundsForReactionConflict FbcFluxBoundsForReactionConflict@endlink</td>
<td class="meaning">Conflicting set of FluxBounds for a reaction</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcObjectiveAllowedL3Attributes FbcObjectiveAllowedL3Attributes@endlink</td>
<td class="meaning"><code>&lt;objective&gt;</code> may only have 'metaId' and 'sboTerm' from L3 namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcObjectiveAllowedElements FbcObjectiveAllowedElements@endlink</td>
<td class="meaning"><code>&lt;objective&gt;</code> may only have <code>&lt;notes&gt;</code> and <code>&lt;annotations&gt;</code> from L3 Core</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcObjectiveRequiredAttributes FbcObjectiveRequiredAttributes@endlink</td>
<td class="meaning">Invalid attribute found on <code>&lt;objective&gt;</code> object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcObjectiveNameMustBeString FbcObjectiveNameMustBeString@endlink</td>
<td class="meaning">The attribute 'fbc:name' must be of the data type string</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcObjectiveTypeMustBeEnum FbcObjectiveTypeMustBeEnum@endlink</td>
<td class="meaning">The attribute 'fbc:type' must be of data type FbcType.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcObjectiveOneListOfObjectives FbcObjectiveOneListOfObjectives@endlink</td>
<td class="meaning">An <code>&lt;objective&gt;</code> must have one <code>&lt;listOfFluxObjectives&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcObjectiveLOFluxObjMustNotBeEmpty FbcObjectiveLOFluxObjMustNotBeEmpty@endlink</td>
<td class="meaning"><code>&lt;listOfFluxObjectives&gt;</code> subobject must not be empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcObjectiveLOFluxObjOnlyFluxObj FbcObjectiveLOFluxObjOnlyFluxObj@endlink</td>
<td class="meaning">Invalid element found in <code>&lt;listOfFluxObjectives&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcObjectiveLOFluxObjAllowedAttribs FbcObjectiveLOFluxObjAllowedAttribs@endlink</td>
<td class="meaning"><code>&lt;listOfFluxObjectives&gt;</code> may only have 'metaId' and 'sboTerm' from L3 core</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxObjectAllowedL3Attributes FbcFluxObjectAllowedL3Attributes@endlink</td>
<td class="meaning"><code>&lt;fluxObjective&gt;</code> may only have 'metaId' and 'sboTerm' from L3 namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxObjectAllowedElements FbcFluxObjectAllowedElements@endlink</td>
<td class="meaning"><code>&lt;fluxObjective&gt;</code> may only have <code>&lt;notes&gt;</code> and <code>&lt;annotations&gt;</code> from L3 Core</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxObjectRequiredAttributes FbcFluxObjectRequiredAttributes@endlink</td>
<td class="meaning">Invalid attribute found on <code>&lt;fluxObjective&gt;</code> object</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxObjectNameMustBeString FbcFluxObjectNameMustBeString@endlink</td>
<td class="meaning">The attribute 'fbc:name' must be of the data type string</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxObjectReactionMustBeSIdRef FbcFluxObjectReactionMustBeSIdRef@endlink</td>
<td class="meaning">Datatype for 'fbc:reaction' must be SIdRef</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxObjectReactionMustExist FbcFluxObjectReactionMustExist@endlink</td>
<td class="meaning">'fbc:reaction' must refer to valid reaction</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#FbcFluxObjectCoefficientMustBeDouble FbcFluxObjectCoefficientMustBeDouble@endlink</td>
<td class="meaning">The attribute 'fbc:coefficient' must be of the data type double</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualUnknown QualUnknown@endlink</td>
<td class="meaning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualNSUndeclared QualNSUndeclared@endlink</td>
<td class="meaning">The qual ns is not correctly declared</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualElementNotInNs QualElementNotInNs@endlink</td>
<td class="meaning">Element not in qual namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualFunctionTermBool QualFunctionTermBool@endlink</td>
<td class="meaning">FunctionTerm should return boolean</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualMathCSymbolDisallowed QualMathCSymbolDisallowed@endlink</td>
<td class="meaning">CSymbol time or delay not allowed</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-warning"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualDuplicateComponentId QualDuplicateComponentId@endlink</td>
<td class="meaning">Duplicate 'id' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualAttributeRequiredMissing QualAttributeRequiredMissing@endlink</td>
<td class="meaning">Required qual:required attribute on <code>&lt;sbml&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualAttributeRequiredMustBeBoolean QualAttributeRequiredMustBeBoolean@endlink</td>
<td class="meaning">The qual:required attribute must be Boolean</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualRequiredTrueIfTransitions QualRequiredTrueIfTransitions@endlink</td>
<td class="meaning">The qual:required attribute must be 'true' if math changes</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualOneListOfTransOrQS QualOneListOfTransOrQS@endlink</td>
<td class="meaning">Only one <code>&lt;listOfTransitions&gt;</code> or <code>&lt;listOfQualitativeSpecies&gt;</code> allowed.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualEmptyLONotAllowed QualEmptyLONotAllowed@endlink</td>
<td class="meaning">Empty <code>&lt;listOfTransitions&gt;</code> or <code>&lt;listOfQualitativeSpecies&gt;</code> not allowed.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualLOTransitiondAllowedElements QualLOTransitiondAllowedElements@endlink</td>
<td class="meaning">Elements allowed on <code>&lt;listOfTransitions&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualLOQualSpeciesAllowedElements QualLOQualSpeciesAllowedElements@endlink</td>
<td class="meaning">Elements allowed on <code>&lt;listOfTransitions&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualLOQualSpeciesAllowedAttributes QualLOQualSpeciesAllowedAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfQualitativeSpecies&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualLOTransitionsAllowedAttributes QualLOTransitionsAllowedAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfTransitions&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualQualSpeciesAllowedCoreAttributes QualQualSpeciesAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;qualitativeSpecies&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualQualSpeciesAllowedElements QualQualSpeciesAllowedElements@endlink</td>
<td class="meaning">Elements allowed on <code>&lt;qualitativeSpecies&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualQualSpeciesAllowedAttributes QualQualSpeciesAllowedAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;qualitativeSpecies&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualConstantMustBeBool QualConstantMustBeBool@endlink</td>
<td class="meaning">Attribute 'constant' on <code>&lt;qualitativeSpecies&gt;</code> must be boolean.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualNameMustBeString QualNameMustBeString@endlink</td>
<td class="meaning">Attribute 'name' on <code>&lt;qualitativeSpecies&gt;</code> must be string.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualInitialLevelMustBeInt QualInitialLevelMustBeInt@endlink</td>
<td class="meaning">Attribute 'initialLevel' on <code>&lt;qualitativeSpecies&gt;</code> must be integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualMaxLevelMustBeInt QualMaxLevelMustBeInt@endlink</td>
<td class="meaning">Attribute 'maxLevel' on <code>&lt;qualitativeSpecies&gt;</code> must be integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualCompartmentMustReferExisting QualCompartmentMustReferExisting@endlink</td>
<td class="meaning">Attribute 'compartment' on <code>&lt;qualitativeSpecies&gt;</code> must reference compartment.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualInitialLevelCannotExceedMax QualInitialLevelCannotExceedMax@endlink</td>
<td class="meaning">Attribute 'initialLevel' on <code>&lt;qualitativeSpecies&gt;</code> cannot exceed maxLevel.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualConstantQSCannotBeOutput QualConstantQSCannotBeOutput@endlink</td>
<td class="meaning">Constant <code>&lt;qualitativeSpecies&gt;</code> cannot be an Output.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualQSAssignedOnlyOnce QualQSAssignedOnlyOnce@endlink</td>
<td class="meaning">A <code>&lt;qualitativeSpecies&gt;</code> can only be assigned once.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualInitalLevelNotNegative QualInitalLevelNotNegative@endlink</td>
<td class="meaning">Attribute 'initialLevel' on <code>&lt;qualitativeSpecies&gt;</code> cannot be negative.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualMaxLevelNotNegative QualMaxLevelNotNegative@endlink</td>
<td class="meaning">Attribute 'maxLevel' on <code>&lt;qualitativeSpecies&gt;</code> cannot be negative.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualTransitionAllowedCoreAttributes QualTransitionAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;transition&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualTransitionAllowedElements QualTransitionAllowedElements@endlink</td>
<td class="meaning">Elements allowed on <code>&lt;transition&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualTransitionAllowedAttributes QualTransitionAllowedAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;transition&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualTransitionNameMustBeString QualTransitionNameMustBeString@endlink</td>
<td class="meaning">Attribute 'name' on <code>&lt;transition&gt;</code> must be string.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualTransitionLOElements QualTransitionLOElements@endlink</td>
<td class="meaning">ListOf elements on <code>&lt;transition&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualTransitionEmptyLOElements QualTransitionEmptyLOElements@endlink</td>
<td class="meaning">ListOf elements on <code>&lt;transition&gt;</code> not empty.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualTransitionLOInputElements QualTransitionLOInputElements@endlink</td>
<td class="meaning">Elements on <code>&lt;listOfInputs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualTransitionLOOutputElements QualTransitionLOOutputElements@endlink</td>
<td class="meaning">Elements on <code>&lt;listOfOutputs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualTransitionLOFuncTermElements QualTransitionLOFuncTermElements@endlink</td>
<td class="meaning">Elements on <code>&lt;listOfFunctionTerms&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualTransitionLOInputAttributes QualTransitionLOInputAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfInputs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualTransitionLOOutputAttributes QualTransitionLOOutputAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfOutputs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualTransitionLOFuncTermAttributes QualTransitionLOFuncTermAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfFunctionTerms&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualTransitionLOFuncTermExceedMax QualTransitionLOFuncTermExceedMax@endlink</td>
<td class="meaning"><code>&lt;listOfFunctionTerms&gt;</code> cannot make qualitativeSpecies exceed maxLevel.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualTransitionLOFuncTermNegative QualTransitionLOFuncTermNegative@endlink</td>
<td class="meaning"><code>&lt;listOfFunctionTerms&gt;</code> cannot make qualitativeSpecies negative.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualInputAllowedCoreAttributes QualInputAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;input&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualInputAllowedElements QualInputAllowedElements@endlink</td>
<td class="meaning">Elements allowed on <code>&lt;input&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualInputAllowedAttributes QualInputAllowedAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;input&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualInputNameMustBeString QualInputNameMustBeString@endlink</td>
<td class="meaning">Attribute 'name' on <code>&lt;input&gt;</code> must be string.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualInputSignMustBeSignEnum QualInputSignMustBeSignEnum@endlink</td>
<td class="meaning">Attribute 'sign' on <code>&lt;input&gt;</code> must be enum.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualInputTransEffectMustBeInputEffect QualInputTransEffectMustBeInputEffect@endlink</td>
<td class="meaning">Attribute 'transitionEffect' on <code>&lt;input&gt;</code> must be enum.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualInputThreshMustBeInteger QualInputThreshMustBeInteger@endlink</td>
<td class="meaning">Attribute 'thresholdLevel' on <code>&lt;input&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualInputQSMustBeExistingQS QualInputQSMustBeExistingQS@endlink</td>
<td class="meaning">Attribute 'qualitativeSpecies' on <code>&lt;input&gt;</code> must refer to existing</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualInputConstantCannotBeConsumed QualInputConstantCannotBeConsumed@endlink</td>
<td class="meaning">Constant <code>&lt;input&gt;</code> cannot be consumed.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualInputThreshMustBeNonNegative QualInputThreshMustBeNonNegative@endlink</td>
<td class="meaning">Attribute 'thresholdLevel' on <code>&lt;input&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualOutputAllowedCoreAttributes QualOutputAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;output&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualOutputAllowedElements QualOutputAllowedElements@endlink</td>
<td class="meaning">Elements allowed on <code>&lt;output&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualOutputAllowedAttributes QualOutputAllowedAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;output&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualOutputNameMustBeString QualOutputNameMustBeString@endlink</td>
<td class="meaning">Attribute 'name' on <code>&lt;output&gt;</code> must be string.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualOutputTransEffectMustBeOutput QualOutputTransEffectMustBeOutput@endlink</td>
<td class="meaning">Attribute 'transitionEffect' on <code>&lt;output&gt;</code> must be enum.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualOutputLevelMustBeInteger QualOutputLevelMustBeInteger@endlink</td>
<td class="meaning">Attribute 'outputLevel' on <code>&lt;output&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualOutputQSMustBeExistingQS QualOutputQSMustBeExistingQS@endlink</td>
<td class="meaning">Attribute 'qualitativeSpecies' on <code>&lt;output&gt;</code> must refer to existing</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualOutputConstantMustBeFalse QualOutputConstantMustBeFalse@endlink</td>
<td class="meaning">Constant 'qualitativeSpecies' cannot be <code>&lt;output&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualOutputProductionMustHaveLevel QualOutputProductionMustHaveLevel@endlink</td>
<td class="meaning"><code>&lt;output&gt;</code> being produced must have level</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualOutputLevelMustBeNonNegative QualOutputLevelMustBeNonNegative@endlink</td>
<td class="meaning">Attribute 'outputLevel' on <code>&lt;output&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualDefaultTermAllowedCoreAttributes QualDefaultTermAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;defaultTerm&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualDefaultTermAllowedElements QualDefaultTermAllowedElements@endlink</td>
<td class="meaning">Elements allowed on <code>&lt;defaultTerm&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualDefaultTermAllowedAttributes QualDefaultTermAllowedAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;defaultTerm&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualDefaultTermResultMustBeInteger QualDefaultTermResultMustBeInteger@endlink</td>
<td class="meaning">Attribute 'resultLevel' on <code>&lt;defaultTerm&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualDefaultTermResultMustBeNonNeg QualDefaultTermResultMustBeNonNeg@endlink</td>
<td class="meaning">Attribute 'resultLevel' on <code>&lt;defaultTerm&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualFuncTermAllowedCoreAttributes QualFuncTermAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;functionTerm&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualFuncTermAllowedElements QualFuncTermAllowedElements@endlink</td>
<td class="meaning">Elements allowed on <code>&lt;functionTerm&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualFuncTermAllowedAttributes QualFuncTermAllowedAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;functionTerm&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualFuncTermOnlyOneMath QualFuncTermOnlyOneMath@endlink</td>
<td class="meaning">Only one <code>&lt;math&gt;</code> on <code>&lt;functionTerm&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualFuncTermResultMustBeInteger QualFuncTermResultMustBeInteger@endlink</td>
<td class="meaning">Attribute 'resultLevel' on <code>&lt;functionTerm&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#QualFuncTermResultMustBeNonNeg QualFuncTermResultMustBeNonNeg@endlink</td>
<td class="meaning">Attribute 'resultLevel' on <code>&lt;functionTerm&gt;</code> must be non negative integer.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutUnknownError LayoutUnknownError@endlink</td>
<td class="meaning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutNSUndeclared LayoutNSUndeclared@endlink</td>
<td class="meaning">The layout ns is not correctly declared</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutElementNotInNs LayoutElementNotInNs@endlink</td>
<td class="meaning">Element not in layout namespace</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutDuplicateComponentId LayoutDuplicateComponentId@endlink</td>
<td class="meaning">Duplicate 'id' attribute value</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSIdSyntax LayoutSIdSyntax@endlink</td>
<td class="meaning">'id' attribute incorrect syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutXsiTypeAllowedLocations LayoutXsiTypeAllowedLocations@endlink</td>
<td class="meaning">'xsi:type' allowed locations</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutXsiTypeSyntax LayoutXsiTypeSyntax@endlink</td>
<td class="meaning">'xsi:type' attribute incorrect syntax</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutAttributeRequiredMissing LayoutAttributeRequiredMissing@endlink</td>
<td class="meaning">Required layout:required attribute on <code>&lt;sbml&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutAttributeRequiredMustBeBoolean LayoutAttributeRequiredMustBeBoolean@endlink</td>
<td class="meaning">The layout:required attribute must be Boolean</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutRequiredFalse LayoutRequiredFalse@endlink</td>
<td class="meaning">The layout:required attribute must be 'false'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutOnlyOneLOLayouts LayoutOnlyOneLOLayouts@endlink</td>
<td class="meaning">Only one listOfLayouts on <code>&lt;model&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOLayoutsNotEmpty LayoutLOLayoutsNotEmpty@endlink</td>
<td class="meaning">ListOf elements cannot be empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOLayoutsAllowedElements LayoutLOLayoutsAllowedElements@endlink</td>
<td class="meaning">Allowed elements on ListOfLayouts</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOLayoutsAllowedAttributes LayoutLOLayoutsAllowedAttributes@endlink</td>
<td class="meaning">Allowed attributes on ListOfLayouts</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLayoutAllowedElements LayoutLayoutAllowedElements@endlink</td>
<td class="meaning">Allowed elements on Layout</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLayoutAllowedCoreAttributes LayoutLayoutAllowedCoreAttributes@endlink</td>
<td class="meaning">Allowed core attributes on Layout</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutOnlyOneEachListOf LayoutOnlyOneEachListOf@endlink</td>
<td class="meaning">Only one each listOf on <code>&lt;layout&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutNoEmptyListOfs LayoutNoEmptyListOfs@endlink</td>
<td class="meaning">ListOf elements cannot be empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLayoutAllowedAttributes LayoutLayoutAllowedAttributes@endlink</td>
<td class="meaning"><code>&lt;layout&gt;</code> must have 'id' and may have 'name'</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLayoutNameMustBeString LayoutLayoutNameMustBeString@endlink</td>
<td class="meaning">'name' must be string</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOCompGlyphAllowedAttributes LayoutLOCompGlyphAllowedAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfCompartmentGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOCompGlyphAllowedElements LayoutLOCompGlyphAllowedElements@endlink</td>
<td class="meaning">Elements allowed on <code>&lt;listOfCompartmentGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOSpeciesGlyphAllowedAttributes LayoutLOSpeciesGlyphAllowedAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfSpeciesGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOSpeciesGlyphAllowedElements LayoutLOSpeciesGlyphAllowedElements@endlink</td>
<td class="meaning">Elements allowed on <code>&lt;listOfSpeciesGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLORnGlyphAllowedAttributes LayoutLORnGlyphAllowedAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfReactionGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLORnGlyphAllowedElements LayoutLORnGlyphAllowedElements@endlink</td>
<td class="meaning">Elements allowed on <code>&lt;listOfReactionGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOAddGOAllowedAttribut LayoutLOAddGOAllowedAttribut@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfAdditionalGraphicalObjectGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOAddGOAllowedElements LayoutLOAddGOAllowedElements@endlink</td>
<td class="meaning">Elements allowed on <code>&lt;listOfAdditionalGraphicalObjectGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLayoutMustHaveDimensions LayoutLayoutMustHaveDimensions@endlink</td>
<td class="meaning">Layout must have <code>&lt;dimensions&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOTextGlyphAllowedAttributes LayoutLOTextGlyphAllowedAttributes@endlink</td>
<td class="meaning">Attributes allowed on <code>&lt;listOfTextGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOTextGlyphAllowedElements LayoutLOTextGlyphAllowedElements@endlink</td>
<td class="meaning">Elements allowed on <code>&lt;listOfTextGlyphs&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGOAllowedCoreElements LayoutGOAllowedCoreElements@endlink</td>
<td class="meaning">Core elements allowed on <code>&lt;graphicalObject&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGOAllowedCoreAttributes LayoutGOAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;graphicalObject&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGOAllowedElements LayoutGOAllowedElements@endlink</td>
<td class="meaning">Layout elements allowed on <code>&lt;graphicalObject&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGOAllowedAttributes LayoutGOAllowedAttributes@endlink</td>
<td class="meaning">Layout attributes allowed on <code>&lt;graphicalObject&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGOMetaIdRefMustBeIDREF LayoutGOMetaIdRefMustBeIDREF@endlink</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGOMetaIdRefMustReferenceObject LayoutGOMetaIdRefMustReferenceObject@endlink</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGOMustContainBoundingBox LayoutGOMustContainBoundingBox@endlink</td>
<td class="meaning">A <code>&lt;graphicalObject&gt;</code> must contain a <code>&lt;boundingBox&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCGAllowedCoreElements LayoutCGAllowedCoreElements@endlink</td>
<td class="meaning">Core elements allowed on <code>&lt;compartmentGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCGAllowedCoreAttributes LayoutCGAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;compartmentGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCGAllowedElements LayoutCGAllowedElements@endlink</td>
<td class="meaning">Layout elements allowed on <code>&lt;compartmentGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCGAllowedAttributes LayoutCGAllowedAttributes@endlink</td>
<td class="meaning">Layout attributes allowed on <code>&lt;compartmentGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCGMetaIdRefMustBeIDREF LayoutCGMetaIdRefMustBeIDREF@endlink</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCGMetaIdRefMustReferenceObject LayoutCGMetaIdRefMustReferenceObject@endlink</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCGCompartmentSyntax LayoutCGCompartmentSyntax@endlink</td>
<td class="meaning">CompartmentGlyph 'compartment' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCGCompartmentMustRefComp LayoutCGCompartmentMustRefComp@endlink</td>
<td class="meaning">CompartmentGlyph compartment must reference existing compartment.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCGNoDuplicateReferences LayoutCGNoDuplicateReferences@endlink</td>
<td class="meaning">CompartmentGlyph cannot reference two objects.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCGOrderMustBeDouble LayoutCGOrderMustBeDouble@endlink</td>
<td class="meaning">CompartmentGlyph order must be double.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSGAllowedCoreElements LayoutSGAllowedCoreElements@endlink</td>
<td class="meaning">Core elements allowed on <code>&lt;speciesGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSGAllowedCoreAttributes LayoutSGAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;speciesGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSGAllowedElements LayoutSGAllowedElements@endlink</td>
<td class="meaning">Layout elements allowed on <code>&lt;speciesGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSGAllowedAttributes LayoutSGAllowedAttributes@endlink</td>
<td class="meaning">Layout attributes allowed on <code>&lt;speciesGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSGMetaIdRefMustBeIDREF LayoutSGMetaIdRefMustBeIDREF@endlink</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSGMetaIdRefMustReferenceObject LayoutSGMetaIdRefMustReferenceObject@endlink</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSGSpeciesSyntax LayoutSGSpeciesSyntax@endlink</td>
<td class="meaning">SpeciesGlyph 'species' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSGSpeciesMustRefSpecies LayoutSGSpeciesMustRefSpecies@endlink</td>
<td class="meaning">SpeciesGlyph species must reference existing species.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSGNoDuplicateReferences LayoutSGNoDuplicateReferences@endlink</td>
<td class="meaning">SpeciesGlyph cannot reference two objects.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutRGAllowedCoreElements LayoutRGAllowedCoreElements@endlink</td>
<td class="meaning">Core elements allowed on <code>&lt;reactionGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutRGAllowedCoreAttributes LayoutRGAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;reactionGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutRGAllowedElements LayoutRGAllowedElements@endlink</td>
<td class="meaning">Layout elements allowed on <code>&lt;reactionGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutRGAllowedAttributes LayoutRGAllowedAttributes@endlink</td>
<td class="meaning">Layout attributes allowed on <code>&lt;reactionGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutRGMetaIdRefMustBeIDREF LayoutRGMetaIdRefMustBeIDREF@endlink</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutRGMetaIdRefMustReferenceObject LayoutRGMetaIdRefMustReferenceObject@endlink</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutRGReactionSyntax LayoutRGReactionSyntax@endlink</td>
<td class="meaning">ReactionGlyph 'reaction' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutRGReactionMustRefReaction LayoutRGReactionMustRefReaction@endlink</td>
<td class="meaning">ReactionGlyph reaction must reference existing reaction.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutRGNoDuplicateReferences LayoutRGNoDuplicateReferences@endlink</td>
<td class="meaning">ReactionGlyph cannot reference two objects.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOSpeciesRefGlyphAllowedElements LayoutLOSpeciesRefGlyphAllowedElements@endlink</td>
<td class="meaning">Allowed elements on ListOfSpeciesReferenceGlyphs</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOSpeciesRefGlyphAllowedAttribs LayoutLOSpeciesRefGlyphAllowedAttribs@endlink</td>
<td class="meaning">Allowed attributes on ListOfSpeciesReferenceGlyphs</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOSpeciesRefGlyphNotEmpty LayoutLOSpeciesRefGlyphNotEmpty@endlink</td>
<td class="meaning">ListOfSpeciesReferenceGlyphs not empty</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGGAllowedCoreElements LayoutGGAllowedCoreElements@endlink</td>
<td class="meaning">Core elements allowed on <code>&lt;generalGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGGAllowedCoreAttributes LayoutGGAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;generalGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGGAllowedElements LayoutGGAllowedElements@endlink</td>
<td class="meaning">Layout elements allowed on <code>&lt;generalGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGGAllowedAttributes LayoutGGAllowedAttributes@endlink</td>
<td class="meaning">Layout attributes allowed on <code>&lt;generalGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGGMetaIdRefMustBeIDREF LayoutGGMetaIdRefMustBeIDREF@endlink</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGGMetaIdRefMustReferenceObject LayoutGGMetaIdRefMustReferenceObject@endlink</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGGReferenceSyntax LayoutGGReferenceSyntax@endlink</td>
<td class="meaning">GeneralGlyph 'reference' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGGReferenceMustRefObject LayoutGGReferenceMustRefObject@endlink</td>
<td class="meaning">GeneralGlyph 'reference' must reference existing element.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutGGNoDuplicateReferences LayoutGGNoDuplicateReferences@endlink</td>
<td class="meaning">GeneralGlyph cannot reference two objects.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOReferenceGlyphAllowedElements LayoutLOReferenceGlyphAllowedElements@endlink</td>
<td class="meaning">Allowed elements on ListOfReferenceGlyphs</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOReferenceGlyphAllowedAttribs LayoutLOReferenceGlyphAllowedAttribs@endlink</td>
<td class="meaning">Allowed attributes on ListOfReferenceGlyphs</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOSubGlyphAllowedElements LayoutLOSubGlyphAllowedElements@endlink</td>
<td class="meaning"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOSubGlyphAllowedAttribs LayoutLOSubGlyphAllowedAttribs@endlink</td>
<td class="meaning">Allowed attributes on ListOfSubGlyphs</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutTGAllowedCoreElements LayoutTGAllowedCoreElements@endlink</td>
<td class="meaning">Core elements allowed on <code>&lt;textGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutTGAllowedCoreAttributes LayoutTGAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;textGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutTGAllowedElements LayoutTGAllowedElements@endlink</td>
<td class="meaning">Layout elements allowed on <code>&lt;textGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutTGAllowedAttributes LayoutTGAllowedAttributes@endlink</td>
<td class="meaning">Layout attributes allowed on <code>&lt;textGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutTGMetaIdRefMustBeIDREF LayoutTGMetaIdRefMustBeIDREF@endlink</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutTGMetaIdRefMustReferenceObject LayoutTGMetaIdRefMustReferenceObject@endlink</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutTGOriginOfTextSyntax LayoutTGOriginOfTextSyntax@endlink</td>
<td class="meaning">TextGlyph 'originOfText' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutTGOriginOfTextMustRefObject LayoutTGOriginOfTextMustRefObject@endlink</td>
<td class="meaning">TextGlyph 'originOfText' must reference existing element.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutTGNoDuplicateReferences LayoutTGNoDuplicateReferences@endlink</td>
<td class="meaning">TextGlyph cannot reference two objects.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutTGGraphicalObjectSyntax LayoutTGGraphicalObjectSyntax@endlink</td>
<td class="meaning">TextGlyph 'graphicalObject' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutTGGraphicalObjectMustRefObject LayoutTGGraphicalObjectMustRefObject@endlink</td>
<td class="meaning">TextGlyph 'graphicalObject' must reference existing element.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutTGTextMustBeString LayoutTGTextMustBeString@endlink</td>
<td class="meaning">TextGlyph 'text' must be string.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSRGAllowedCoreElements LayoutSRGAllowedCoreElements@endlink</td>
<td class="meaning">Core elements allowed on <code>&lt;speciesReferenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSRGAllowedCoreAttributes LayoutSRGAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;speciesReferenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSRGAllowedElements LayoutSRGAllowedElements@endlink</td>
<td class="meaning">Layout elements allowed on <code>&lt;speciesReferenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSRGAllowedAttributes LayoutSRGAllowedAttributes@endlink</td>
<td class="meaning">Layout attributes allowed on <code>&lt;speciesReferenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSRGMetaIdRefMustBeIDREF LayoutSRGMetaIdRefMustBeIDREF@endlink</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSRGMetaIdRefMustReferenceObject LayoutSRGMetaIdRefMustReferenceObject@endlink</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSRGSpeciesReferenceSyntax LayoutSRGSpeciesReferenceSyntax@endlink</td>
<td class="meaning">SpeciesReferenceGlyph 'speciesReference' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSRGSpeciesRefMustRefObject LayoutSRGSpeciesRefMustRefObject@endlink</td>
<td class="meaning">SpeciesReferenceGlyph 'speciesReference' must reference existing element.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSRGNoDuplicateReferences LayoutSRGNoDuplicateReferences@endlink</td>
<td class="meaning">SpeciesReferenceGlyph cannot reference two objects.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSRGSpeciesGlyphSyntax LayoutSRGSpeciesGlyphSyntax@endlink</td>
<td class="meaning">SpeciesReferenceGlyph 'speciesGlyph' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSRGSpeciesGlyphMustRefObject LayoutSRGSpeciesGlyphMustRefObject@endlink</td>
<td class="meaning">SpeciesReferenceGlyph 'speciesGlyph' must reference existing element.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutSRGRoleSyntax LayoutSRGRoleSyntax@endlink</td>
<td class="meaning">SpeciesReferenceGlyph 'role' must be string from enumeration.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutREFGAllowedCoreElements LayoutREFGAllowedCoreElements@endlink</td>
<td class="meaning">Core elements allowed on <code>&lt;referenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutREFGAllowedCoreAttributes LayoutREFGAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;referenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutREFGAllowedElements LayoutREFGAllowedElements@endlink</td>
<td class="meaning">Layout elements allowed on <code>&lt;referenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutREFGAllowedAttributes LayoutREFGAllowedAttributes@endlink</td>
<td class="meaning">Layout attributes allowed on <code>&lt;referenceGlyph&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutREFGMetaIdRefMustBeIDREF LayoutREFGMetaIdRefMustBeIDREF@endlink</td>
<td class="meaning">Layout 'metIdRef' must be IDREF.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutREFGMetaIdRefMustReferenceObject LayoutREFGMetaIdRefMustReferenceObject@endlink</td>
<td class="meaning">Layout 'metIdRef' must reference existing object.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutREFGReferenceSyntax LayoutREFGReferenceSyntax@endlink</td>
<td class="meaning">ReferenceGlyph 'reference' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutREFGReferenceMustRefObject LayoutREFGReferenceMustRefObject@endlink</td>
<td class="meaning">ReferenceGlyph 'reference' must reference existing element.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutREFGNoDuplicateReferences LayoutREFGNoDuplicateReferences@endlink</td>
<td class="meaning">ReferenceGlyph cannot reference two objects.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutREFGGlyphSyntax LayoutREFGGlyphSyntax@endlink</td>
<td class="meaning">ReferenceGlyph 'glyph' must have SIdRef syntax.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutREFGGlyphMustRefObject LayoutREFGGlyphMustRefObject@endlink</td>
<td class="meaning">ReferenceGlyph 'glyph' must reference existing element.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutREFGRoleSyntax LayoutREFGRoleSyntax@endlink</td>
<td class="meaning">ReferenceGlyph 'role' must be string.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutPointAllowedCoreElements LayoutPointAllowedCoreElements@endlink</td>
<td class="meaning">Core elements allowed on <code>&lt;point&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutPointAllowedCoreAttributes LayoutPointAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;point&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutPointAllowedAttributes LayoutPointAllowedAttributes@endlink</td>
<td class="meaning">Layout attributes allowed on <code>&lt;point&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutPointAttributesMustBeDouble LayoutPointAttributesMustBeDouble@endlink</td>
<td class="meaning">Layout 'x', 'y' and 'z' must be double.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutBBoxAllowedCoreElements LayoutBBoxAllowedCoreElements@endlink</td>
<td class="meaning">Core elements allowed on <code>&lt;boundingBox&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutBBoxAllowedCoreAttributes LayoutBBoxAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;boundingBox&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutBBoxAllowedElements LayoutBBoxAllowedElements@endlink</td>
<td class="meaning">Layout elements allowed on <code>&lt;boundingBox&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutBBoxAllowedAttributes LayoutBBoxAllowedAttributes@endlink</td>
<td class="meaning">Layout attributes allowed on <code>&lt;boundingBox&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutBBoxConsistent3DDefinition LayoutBBoxConsistent3DDefinition@endlink</td>
<td class="meaning">Layout consistent dimensions on a <code>&lt;boundingBox&gt;</code></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCurveAllowedCoreElements LayoutCurveAllowedCoreElements@endlink</td>
<td class="meaning">Core elements allowed on <code>&lt;curve&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCurveAllowedCoreAttributes LayoutCurveAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;curve&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCurveAllowedElements LayoutCurveAllowedElements@endlink</td>
<td class="meaning">Layout elements allowed on <code>&lt;curve&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCurveAllowedAttributes LayoutCurveAllowedAttributes@endlink</td>
<td class="meaning">Layout attributes allowed on <code>&lt;curve&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOCurveSegsAllowedAttributes LayoutLOCurveSegsAllowedAttributes@endlink</td>
<td class="meaning">Allowed attributes on ListOfCurveSegments</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOCurveSegsAllowedElements LayoutLOCurveSegsAllowedElements@endlink</td>
<td class="meaning">Allowed elements on ListOfCurveSegments</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLOCurveSegsNotEmpty LayoutLOCurveSegsNotEmpty@endlink</td>
<td class="meaning">No empty ListOfCurveSegments</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLSegAllowedCoreElements LayoutLSegAllowedCoreElements@endlink</td>
<td class="meaning">Core elements allowed on <code>&lt;lineSegment&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLSegAllowedCoreAttributes LayoutLSegAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;lineSegment&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLSegAllowedElements LayoutLSegAllowedElements@endlink</td>
<td class="meaning">Layout elements allowed on <code>&lt;lineSegment&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutLSegAllowedAttributes LayoutLSegAllowedAttributes@endlink</td>
<td class="meaning">Layout attributes allowed on <code>&lt;lineSegment&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCBezAllowedCoreElements LayoutCBezAllowedCoreElements@endlink</td>
<td class="meaning">Core elements allowed on <code>&lt;cubicBezier&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCBezAllowedCoreAttributes LayoutCBezAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;cubicBezier&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCBezAllowedElements LayoutCBezAllowedElements@endlink</td>
<td class="meaning">Layout elements allowed on <code>&lt;cubicBezier&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutCBezAllowedAttributes LayoutCBezAllowedAttributes@endlink</td>
<td class="meaning">Layout attributes allowed on <code>&lt;cubicBezier&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutDimsAllowedCoreElements LayoutDimsAllowedCoreElements@endlink</td>
<td class="meaning">Core elements allowed on <code>&lt;dimensions&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutDimsAllowedCoreAttributes LayoutDimsAllowedCoreAttributes@endlink</td>
<td class="meaning">Core attributes allowed on <code>&lt;dimensions&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutDimsAllowedAttributes LayoutDimsAllowedAttributes@endlink</td>
<td class="meaning">Layout attributes allowed on <code>&lt;dimensions&gt;</code>.</td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-na"></td>
<td class="s-error"></td>
</tr>
<tr><td class="code">@link SBMLErrorCode_t#LayoutDimsAttributesMustBeDouble LayoutDimsAttributesMustBeDouble@endlink</td>
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
 * ConversionProperties::addOption() on this object with one arguments: a
 * text string that identifies the desired converter.  (The text string is
 * specific to each converter; consult the documentation for a given
 * converter to find out how it should be enabled.)
 *
 * Next, for some converters, the caller can optionally set some
 * converter-specific properties using additional calls to
 * ConversionProperties::addOption().  Many converters provide the ability to
 * configure their behavior to some extent; this is realized through the use
 * of properties that offer different options.  The default property values
 * for each converter can be interrogated using the method
 * SBMLConverter::getDefaultProperties() on the converter class in question .
 *
 * Finally, the caller should invoke the method SBMLDocument::convert()
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
