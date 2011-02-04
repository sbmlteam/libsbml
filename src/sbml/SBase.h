/**
 * @file    SBase.h
 * @brief   Definition of SBase, the base object of all SBML objects
 * @author  Ben Bornstein
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
 * @class SBase
 * @brief LibSBML implementation of %SBase, the base class of most SBML objects.
 *
 * Most components in SBML are derived from a single abstract base type,
 * SBase.  In addition to serving as the parent class for most other
 * classes of objects in SBML, this base type is designed to allow a
 * modeler or a software package to attach arbitrary information to each
 * major element or list in an SBML model.
 *
 * SBase has an optional subelement called "notes".  It is intended to
 * serve as a place for storing optional information intended to be seen by
 * humans.  An example use of the "notes" element would be to contain
 * formatted user comments about the model element in which the "notes"
 * element is enclosed.  There are certain conditions on the XHTML content
 * permitted inside the "notes" element; please consult the <a
 * target="_blank" href="http://sbml.org/Documents/Specifications">SBML
 * specification document</a> corresponding to the SBML Level and Version
 * of your model for more information about the requirements for "notes"
 * content.
 *
 * SBase has another optional subelement called "annotation".  Whereas the
 * "notes" element described above is a container for content to be shown
 * directly to humans, the "annotation" element is a container for optional
 * software-generated content @em not meant to be shown to humans.  The
 * element's content type is <a target="_blank"
 * href="http://www.w3.org/TR/2004/REC-xml-20040204/#elemdecls">XML type
 * "any"</a>, allowing essentially arbitrary data content.  SBML places
 * only a few restrictions on the organization of the content; these are
 * intended to help software tools read and write the data as well as help
 * reduce conflicts between annotations added by different tools.  As is
 * the case with "notes", it is important to refer to the <a
 * target="_blank" href="http://sbml.org/Documents/Specifications">SBML
 * specification document</a> corresponding to the SBML Level and Version
 * of your model for more information about the requirements for
 * "annotation" content.
 * 
 * It is worth pointing out that the "annotation" element in the definition
 * of SBase exists in order that software developers may attach optional
 * application-specific data to the elements in an SBML model.  However, it
 * is important that this facility not be misused.  In particular, it is
 * <em>critical</em> that data essential to a model definition or that can
 * be encoded in existing SBML elements is <em>not</em> stored in
 * "annotation". Parameter values, functional dependencies between model
 * elements, etc., should not be recorded as annotations.  It is crucial to
 * keep in mind the fact that data placed in annotations can be freely
 * ignored by software applications.  If such data affects the
 * interpretation of a model, then software interoperability is greatly
 * impeded.
 *
 * SBML Level 2 introduced an optional SBase attribute named "metaid" for
 * supporting metadata annotations using RDF (<a target="_blank"
 * href="http://www.w3.org/RDF/">Resource Description Format</a>). The
 * attribute value has the data type <a
 * href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>, the XML identifier
 * type, which means each "metaid" value must be globally unique within an
 * SBML file.  (Importantly, this uniqueness criterion applies across any
 * attribute with type <a href="http://www.w3.org/TR/REC-xml/#id">XML
 * ID</a>, not just the "metaid" attribute used by SBML&mdash;something to
 * be aware of if your application-specific XML content inside the
 * "annotation" subelement happens to use <a
 * href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>.)  The "metaid" value
 * serves to identify a model component for purposes such as referencing
 * that component from metadata placed within "annotation" subelements.
 *
 * Beginning with SBML Level 2 Version 3, SBase also has an optional
 * attribute named "sboTerm" for supporting the use of the Systems Biology
 * Ontology.  In SBML proper, the data type of the attribute is a string of
 * the form "SBO:NNNNNNN", where "NNNNNNN" is a seven digit integer number;
 * libSBML simplifies the representation by only storing the "NNNNNNN"
 * integer portion.  Thus, in libSBML, the "sboTerm" attribute on SBase has
 * data type @c int, and SBO identifiers are stored simply as integers.
 * (For convenience, SBase offers methods for returning both the integer
 * form and a text-string form of the SBO identifier.)  SBO terms are a
 * type of optional annotation, and each different class of SBML object
 * derived from SBase imposes its own requirements about the values
 * permitted for "sboTerm".  Please consult the SBML Level&nbsp;2
 * Version&nbsp;4 specification for more information about the use of SBO
 * and the "sboTerm" attribute.
 *
 * Finally, note that, in the list of methods on SBase, there is no public
 * constructor because SBase is an abstract class.  The constructors reside
 * in the subclasses derived from SBase.
 *
 *
 * @section sbase-miriam Standard format for annotations linking data resources
 *
 * SBML Level 2 Versions 2, 3 and 4 define a proposed regular format for
 * encoding two particular categories of annotations: (a) references to
 * controlled vocabulary terms and database identifiers which define and
 * describe biological and biochemical entities in a model; and (b)
 * descriptions of the provenance of a model, including its author(s) and
 * modification history.
 */


#ifndef SBase_h
#define SBase_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/SBMLTypeCodes.h>
#include <sbml/annotation/RDFAnnotation.h>
#include <sbml/annotation/CVTerm.h>
#include <sbml/util/List.h>
#include <sbml/SBMLNamespaces.h>
#include <sbml/SyntaxChecker.h>


#ifdef __cplusplus


#include <string>
#include <stdexcept>

LIBSBML_CPP_NAMESPACE_BEGIN

class SBMLErrorLog;
class SBMLVisitor;
class SBMLDocument;
class Model;

class List;

class XMLAttributes;
class XMLInputStream;
class XMLNode;
class XMLNamespaces;
class XMLOutputStream;
class XMLToken;

/** @cond doxygen-libsbml-internal */
class SBMLConstructorException : public std::invalid_argument
{
public:

  /* constructor */
  SBMLConstructorException (std::string 
                    message="Level/version/namespaces combination is invalid");
};
/** @endcond */


class LIBSBML_EXTERN SBase
{
public:

  /**
   * Destroys this object.
   */
  virtual ~SBase ();


  /**
   * Assignment operator for SBase.
   */
  SBase& operator=(const SBase& rhs);


  /**
   * Accepts the given SBMLVisitor for this SBase object.
   *
   * @param v the SBMLVisitor instance to be used
   *
   * @return the result of calling <code>v.visit()</code>.
   */
  virtual bool accept (SBMLVisitor& v) const = 0;


  /**
   * Creates and returns a deep copy of this SBase object.
   * 
   * @return a (deep) copy of this SBase object.
   */
  virtual SBase* clone () const = 0;


  /**
   * Returns the value of the "metaid" attribute of this object.
   * 
   * The optional attribute named "metaid", present on every major SBML
   * component type, is for supporting metadata annotations using RDF
   * (Resource Description Format). The attribute value has the data type
   * <a href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>, the XML
   * identifier type, which means each "metaid" value must be globally
   * unique within an SBML file.  (Importantly, this uniqueness criterion
   * applies across any attribute with type <a
   * href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>, not just the
   * "metaid" attribute used by SBML&mdash;something to be aware of if your
   * application-specific XML content inside the "annotation" subelement
   * happens to use <a href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>.)
   * The "metaid" value serves to identify a model
   * component for purposes such as referencing that component from
   * metadata placed within "annotation" subelements.
   *  
   * @return the metaid of this SBML object.
   *
   * @see isSetMetaId()
   * @see setMetaId(const std::string& metaid)
   */
  const std::string& getMetaId () const;


  /**
   * Returns the value of the "metaid" attribute of this object.
   * 
   * The optional attribute named "metaid", present on every major SBML
   * component type, is for supporting metadata annotations using RDF
   * (Resource Description Format).  The attribute value has the data type
   * <a href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>, the XML
   * identifier type, which means each "metaid" value must be globally
   * unique within an SBML file.  (Importantly, this uniqueness criterion
   * applies across any attribute with type <a
   * href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>, not just the
   * "metaid" attribute used by SBML&mdash;something to be aware of if your
   * application-specific XML content inside the "annotation" subelement
   * happens to use <a href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>.)
   * The "metaid" value serves to identify a model
   * component for purposes such as referencing that component from
   * metadata placed within "annotation" subelements.
   *  
   * @return the metaid of this SBML object, as a string.
   *
   * @see isSetMetaId()
   * @see setMetaId(const std::string& metaid)
   */
  std::string& getMetaId ();


  /** @cond doxygen-libsbml-internal */
  /*
   * NOTE: THIS IS FOR BACKWARD COMPATABILITY REASONS
   *
   * Returns the value of the "id" attribute of this object, if it has one.
   * 
   * Most (but not all) objects in SBML include two common attributes: "id"
   * and "name".  The identifier given by an object's "id" attribute value
   * is used to identify the object within the SBML model definition.
   * Other objects can refer to the component using this identifier.  The
   * data type of "id" is always either <code>Sid</code> or
   * <code>UnitSId</code>, depending on the object in question.  Both
   * data types are defined as follows:
   * @code
   *   letter ::= 'a'..'z','A'..'Z'
   *   digit  ::= '0'..'9'
   *   idChar ::= letter | digit | '_'
   *   SId    ::= ( letter | '_' ) idChar*
   * @endcode
   *
   * The equality of <code>SId</code> and <code>UnitSId</code> type values
   * in SBML is determined by an exact character sequence match; i.e.,
   * comparisons of these identifiers must be performed in a case-sensitive
   * manner.  This applies to all uses of <code>SId</code> and
   * <code>UnitSId</code>.
   *
   * @return the id of this SBML object.
   *
   * @note The fact that the value of attribute "id" is defined on the
   * SBase parent class object is a convenience provided by libSBML, and
   * <b>does not strictly follow SBML specifications</b>.  This libSBML
   * implementation of SBase allows client applications to use more
   * generalized code in some situations (for instance, when manipulating
   * objects that are all known to have identifiers), but beware that not
   * all SBML object classes provide an "id" attribute.  LibSBML will allow
   * the identifier to be set, but it will not read nor write "id"
   * attributes for objects that do not possess them according to the SBML
   * specification for the Level and Version in use.
   *
   * @see setId(const std::string& sid)
   * @see isSetId()
   * @see unsetId()
   */
  const std::string& getId () const;
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /*
   * NOTE: THIS IS FOR BACKWARD COMPATABILITY REASONS
   *
   * Returns the value of the "name" attribute of this object, if it has one.
   * 
   * Most (but not all) objects in SBML include two common attributes: "id"
   * and "name".  In contrast to the "id" attribute, the "name" attribute is
   * optional and is not intended to be used for cross-referencing purposes
   * within a model.  Its purpose instead is to provide a human-readable
   * label for the component.  The data type of "name" is the type
   * <code>string</code> defined in XML Schema.  SBML imposes no
   * restrictions as to the content of "name" attributes beyond those
   * restrictions defined by the <code>string</code> type in XML Schema.
   *
   * The recommended practice for handling "name" is as follows.  If a
   * software tool has the capability for displaying the content of "name"
   * attributes, it should display this content to the user as a
   * component's label instead of the component's "id".  If the user
   * interface does not have this capability (e.g., because it cannot
   * display or use special characters in symbol names), or if the "name"
   * attribute is missing on a given component, then the user interface
   * should display the value of the "id" attribute instead.  (Script
   * language interpreters are especially likely to display "id" instead of
   * "name".)
   * 
   * As a consequence of the above, authors of systems that automatically
   * generate the values of "id" attributes should be aware some systems
   * may display the "id"'s to the user.  Authors therefore may wish to
   * take some care to have their software create "id" values that are: (a)
   * reasonably easy for humans to type and read; and (b) likely to be
   * meaningful, for example by making the "id" attribute be an abbreviated
   * form of the name attribute value.
   * 
   * An additional point worth mentioning is although there are
   * restrictions on the uniqueness of "id" values, there are no
   * restrictions on the uniqueness of "name" values in a model.  This
   * allows software packages leeway in assigning component identifiers.
   * 
   * @return the name of this SBML object.
   *
   * @note The fact that the "name" attribute is defined on the SBase parent
   * class object is a convenience provided by libSBML, and <b>does not
   * strictly follow SBML specifications</b>.  This libSBML implementation
   * of SBase allows client applications to use more generalized code in
   * some situations (for instance, when manipulating objects that are all
   * known to have identifiers), but beware that not all SBML object
   * classes provide an "id" attribute.  LibSBML will allow the identifier
   * to be set, but it will not read nor write "id" attributes for objects
   * that do not possess them according to the SBML specification for the
   * Level and Version in use.
   *
   * @see isSetName()
   * @see setName(const std::string& name)
   * @see unsetName()
   */
  const std::string& getName () const;
  /** @endcond */


  /**
   * Returns the content of the "notes" subelement of this object as
   * a tree of XMLNode objects.
   *
   * The optional SBML element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments about the
   * model element in which the "notes" element is enclosed.  Every object
   * derived directly or indirectly from type SBase can have a separate
   * value for "notes", allowing users considerable freedom when adding
   * comments to their models.
   *
   * The format of "notes" elements must be <a target="_blank"
   * href="http://www.w3.org/TR/xhtml1/">XHTML&nbsp;1.0</a>.  To help
   * verify the formatting of "notes" content, libSBML provides the static
   * utility method SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif); however,
   * readers are urged to consult the appropriate <a target="_blank"
   * href="http://sbml.org/Documents/Specifications">SBML specification
   * document</a> for the Level and Version of their model for more
   * in-depth explanations.  The SBML Level&nbsp;2 and &nbsp;3
   * specifications have considerable detail about how "notes" element
   * content must be structured.
   *
   * The "notes" element content returned by this method will be in XML
   * form, but libSBML does not provide an object model specifically for
   * the content of notes.  Callers will need to traverse the XML tree
   * structure using the facilities available on XMLNode and related
   * objects.  For an alternative method of accessing the notes, see
   * getNotesString().
   *
   * @return the content of the "notes" subelement of this SBML object as a
   * tree structure composed of XMLNode objects.
   *
   * @see getNotesString()
   * @see isSetNotes()
   * @see setNotes(const XMLNode* notes)
   * @see setNotes(const std::string& notes)
   * @see appendNotes(const XMLNode* notes)
   * @see appendNotes(const std::string& notes)
   * @see unsetNotes()
   * @see SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif)
   */
  XMLNode* getNotes();


  /**
   * Returns the content of the "notes" subelement of this object as a
   * string.
   *
   * The optional SBML element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments about the
   * model element in which the "notes" element is enclosed.  Every object
   * derived directly or indirectly from type SBase can have a separate
   * value for "notes", allowing users considerable freedom when adding
   * comments to their models.
   *
   * The format of "notes" elements must be <a target="_blank"
   * href="http://www.w3.org/TR/xhtml1/">XHTML&nbsp;1.0</a>.  To help
   * verify the formatting of "notes" content, libSBML provides the static
   * utility method SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif); however,
   * readers are urged to consult the appropriate <a target="_blank"
   * href="http://sbml.org/Documents/Specifications">SBML specification
   * document</a> for the Level and Version of their model for more
   * in-depth explanations.  The SBML Level&nbsp;2 and &nbsp;3
   * specifications have considerable detail about how "notes" element
   * content must be structured.
   *
   * For an alternative method of accessing the notes, see getNotes(),
   * which returns the content as an XMLNode tree structure.  Depending on
   * an application's needs, one or the other method may be more
   * convenient.
   *
   * @return the content of the "notes" subelement of this SBML object as a
   * string.
   *
   * @see getNotes()
   * @see isSetNotes()
   * @see setNotes(const XMLNode* notes)
   * @see setNotes(const std::string& notes)
   * @see appendNotes(const XMLNode* notes)
   * @see appendNotes(const std::string& notes)
   * @see unsetNotes()
   * @see SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif)
   */
  std::string getNotesString ();


  /**
   * Returns the content of the "annotation" subelement of this object as
   * a tree of XMLNode objects.
   *
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
   * The annotations returned by this method will be in XML form.  LibSBML
   * provides an object model and related interfaces for certain specific
   * kinds of annotations, namely model history information and RDF
   * content.  See the ModelHistory, CVTerm and RDFAnnotationParser classes
   * for more information about the facilities available.
   *
   * @return the annotation of this SBML object as a tree of XMLNode objects.
   *
   * @see getAnnotationString()
   * @see isSetAnnotation()
   * @see setAnnotation(const XMLNode* annotation)
   * @see setAnnotation(const std::string& annotation)
   * @see appendAnnotation(const XMLNode* annotation)
   * @see appendAnnotation(const std::string& annotation)
   * @see unsetAnnotation()
   */
  XMLNode* getAnnotation ();


  /**
   * Returns the content of the "annotation" subelement of this object as a
   * character string.
   *
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
   * The annotations returned by this method will be in string form.
   *
   * @return the annotation of this SBML object as a character string.
   *
   * @see getAnnotation()
   * @see isSetAnnotation()
   * @see setAnnotation(const XMLNode* annotation)
   * @see setAnnotation(const std::string& annotation)
   * @see appendAnnotation(const XMLNode* annotation)
   * @see appendAnnotation(const std::string& annotation)
   * @see unsetAnnotation()
   */
  std::string getAnnotationString ();


  /**
   * Returns a list of the XML Namespaces declared on this SBML document.
   * 
   * The SBMLNamespaces object encapsulates SBML Level/Version/namespaces
   * information.  It is used to communicate the SBML Level, Version, and
   * (in SBML Level&nbsp;3) packages used in addition to SBML Level&nbsp;3
   * Core.
   * 
   * @return the XML Namespaces associated with this SBML object
   *
   * @see getLevel()
   * @see getVersion()
   */
  virtual XMLNamespaces* getNamespaces() const ;


  /**
   * Returns the SBMLDocument object containing @em this object instance.
   *
   * LibSBML uses the class SBMLDocument as a top-level container for
   * storing SBML content and data associated with it (such as warnings and
   * error messages).  An SBML model in libSBML is contained inside an
   * SBMLDocument object.  SBMLDocument corresponds roughly to the class
   * <i>SBML</i> defined in the SBML Level&nbsp;3 and Level&nbsp;2
   * specifications, but it does not have a direct correspondence in SBML
   * Level&nbsp;1.  (But, it is created by libSBML no matter whether the
   * model is Level&nbsp;1, Level&nbsp;2 or Level&nbsp;3.)
   *
   * This method allows the caller to obtain the SBMLDocument for the
   * current object.
   * 
   * @return the parent SBMLDocument object of this SBML object.
   *
   * @see getParentSBMLObject()
   * @see getModel()
   */
  const SBMLDocument* getSBMLDocument () const;


  /**
   * Returns the SBMLDocument object containing @em this object instance.
   *
   * LibSBML uses the class SBMLDocument as a top-level container for
   * storing SBML content and data associated with it (such as warnings and
   * error messages).  An SBML model in libSBML is contained inside an
   * SBMLDocument object.  SBMLDocument corresponds roughly to the class
   * <i>SBML</i> defined in the SBML Level&nbsp;3 and Level&nbsp;2
   * specifications, but it does not have a direct correspondence in SBML
   * Level&nbsp;1.  (But, it is created by libSBML no matter whether the
   * model is Level&nbsp;1, Level&nbsp;2 or Level&nbsp;3.)
   *
   * This method allows the caller to obtain the SBMLDocument for the
   * current object.
   * 
   * @return the parent SBMLDocument object of this SBML object.
   *
   * @see getParentSBMLObject()
   * @see getModel()
   */
  SBMLDocument* getSBMLDocument ();


  /**
   * Returns the parent SBML object containing this object.
   *
   * This returns the immediately-containing object.  This method is
   * convenient when holding an object nested inside other objects in an
   * SBML model.  
   * 
   * @return the parent SBML object of this SBML object.
   *
   * @see getSBMLDocument()
   * @see getModel()
   */
  SBase* getParentSBMLObject();


  /**
   * Returns the ancestor SBML object that corresponds to the given
   * SBML type code value.
   *
   * This function allows any object to determine its exact
   * location/function within a model.  For example, in SBML Level&nbsp;2 a
   * StoichiometryMath object has ancestors (in order) of type
   * SpeciesReference, ListOfSpeciesReferences, Reaction, ListOfReactions
   * and Model, any of which can be accessed via this function.
   *
   * @param type the @if clike #SBMLTypeCode_t value@endif@if java SBML object type code@endif@if python SBML object type code@endif of the ancestor being sought.
   * 
   * @return the ancestor SBML object of this SBML object that corresponds
   * to the given @if clike #SBMLTypeCode_t value@endif@if notcpp SBML object type code@endif, or @c NULL if none exists.
   */
  SBase* getAncestorOfType(SBMLTypeCode_t type);


  /**
   * Returns the integer portion of the value of the "sboTerm" attribute of
   * this object.
   *
   * Beginning with SBML Level 2 Version 3, objects derived from SBase have
   * an optional attribute named "sboTerm" for supporting the use of the
   * Systems Biology Ontology.  In SBML proper, the data type of the
   * attribute is a string of the form "SBO:NNNNNNN", where "NNNNNNN" is a
   * seven digit integer number; libSBML simplifies the representation by
   * only storing the "NNNNNNN" integer portion.  Thus, in libSBML, the
   * "sboTerm" attribute on SBase has data type @c int, and SBO identifiers
   * are stored simply as integers.  (For convenience, libSBML offers
   * methods for returning both the integer form and a text-string form of
   * the SBO identifier.)
   *
   * SBO terms are a type of optional annotation, and each different class
   * of SBML object derived from SBase imposes its own requirements about
   * the values permitted for "sboTerm".  Please consult the SBML
   * Level&nbsp;2 Version&nbsp;4 specification for more information about
   * the use of SBO and the "sboTerm" attribute.
   *
   * @return the value of the "sboTerm" attribute as an integer, or @c -1
   * if the value is not set.
   */
  int getSBOTerm () const;


  /**
   * Returns the string representation of the "sboTerm" attribute of
   * this object.
   *
   * Beginning with SBML Level 2 Version 3, objects derived from SBase have
   * an optional attribute named "sboTerm" for supporting the use of the
   * Systems Biology Ontology.  In SBML proper, the data type of the
   * attribute is a string of the form "SBO:NNNNNNN", where "NNNNNNN" is a
   * seven digit integer number; libSBML simplifies the representation by
   * only storing the "NNNNNNN" integer portion.  Thus, in libSBML, the
   * "sboTerm" attribute on SBase has data type @c int, and SBO identifiers
   * are stored simply as integers.  This method returns the entire SBO
   * identifier as a text string in the form "SBO:NNNNNNN".
   *
   * SBO terms are a type of optional annotation, and each different class
   * of SBML object derived from SBase imposes its own requirements about
   * the values permitted for "sboTerm".  Please consult the SBML
   * Level&nbsp;2 Version&nbsp;4 specification for more information about
   * the use of SBO and the "sboTerm" attribute.
   *
   * @return the value of the "sboTerm" attribute as a string (its value
   * will be of the form "SBO:NNNNNNN"), or an empty string if
   * the value is not set.
   */
  std::string getSBOTermID () const;


  /**
   * Returns the line number on which this object first appears in the XML
   * representation of the SBML document.
   * 
   * @return the line number of this SBML object.
   *
   * @note The line number for each construct in an SBML model is set upon
   * reading the model.  The accuracy of the line number depends on the
   * correctness of the XML representation of the model, and on the
   * particular XML parser library being used.  The former limitation
   * relates to the following problem: if the model is actually invalid
   * XML, then the parser may not be able to interpret the data correctly
   * and consequently may not be able to establish the real line number.
   * The latter limitation is simply that different parsers seem to have
   * their own accuracy limitations, and out of all the parsers supported
   * by libSBML, none have been 100% accurate in all situations. (At this
   * time, libSBML supports the use of <a target="_blank"
   * href="http://xmlsoft.org">libxml2</a>, <a target="_blank"
   * href="http://expat.sourceforge.net/">Expat</a> and <a target="_blank"
   * href="http://http://xerces.apache.org/xerces-c/">Xerces</a>.)
   *
   * @see getColumn()
   */
  unsigned int getLine () const;


  /**
   * Returns the column number on which this object first appears in the XML
   * representation of the SBML document.
   * 
   * @return the column number of this SBML object.
   * 
   * @note The column number for each construct in an SBML model is set
   * upon reading the model.  The accuracy of the column number depends on
   * the correctness of the XML representation of the model, and on the
   * particular XML parser library being used.  The former limitation
   * relates to the following problem: if the model is actually invalid
   * XML, then the parser may not be able to interpret the data correctly
   * and consequently may not be able to establish the real column number.
   * The latter limitation is simply that different parsers seem to have
   * their own accuracy limitations, and out of all the parsers supported
   * by libSBML, none have been 100% accurate in all situations. (At this
   * time, libSBML supports the use of <a target="_blank"
   * href="http://xmlsoft.org">libxml2</a>, <a target="_blank"
   * href="http://expat.sourceforge.net/">Expat</a> and <a target="_blank"
   * href="http://http://xerces.apache.org/xerces-c/">Xerces</a>.)
   * 
   * @see getLine()
   */
  unsigned int getColumn () const;


  /**
   * Returns the ModelHistory object, if any, attached to this object.
   *
   * @return the ModelHistory object attached to this object, or @c NULL if
   * none exist.
   * 
   * @note In SBML Level&nbsp;2, model history annotations were only
   * permitted on the Model element.  In SBML Level&nbsp;3, they are
   * permitted on all SBML components derived from SBase.
   */
  ModelHistory* getModelHistory() const;


  /**
   * Returns the ModelHistory object, if any, attached to this object.
   * 
   * @return the ModelHistory object attached to this object, or @c NULL if
   * none exist.
   * 
   * @note In SBML Level&nbsp;2, model history annotations were only
   * permitted on the Model element.  In SBML Level&nbsp;3, they are
   * permitted on all SBML components derived from SBase.
   */
  ModelHistory* getModelHistory();


  /**
   * Predicate returning @c true if this
   * object's "metaid" attribute is set.
   *
   * The optional attribute named "metaid", present on every major SBML
   * component type, is for supporting metadata annotations using RDF
   * (Resource Description Format). The attribute value has the data type
   * <a href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>, the XML
   * identifier type, which means each "metaid" value must be globally
   * unique within an SBML file.  (Importantly, this uniqueness criterion
   * applies across any attribute with type <a
   * href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>, not just the
   * "metaid" attribute used by SBML&mdash;something to be aware of if your
   * application-specific XML content inside the "annotation" subelement
   * happens to use <a href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>.)
   * The "metaid" value serves to identify a model component for purposes
   * such as referencing that component from metadata placed within
   * "annotation" subelements.
   *
   * @return @c true if the "metaid" attribute of this SBML object is
   * set, @c false otherwise.
   *
   * @see getMetaId()
   * @see setMetaId(const std::string& metaid)
   */
  bool isSetMetaId () const;


  /** @cond doxygen-libsbml-internal */
  /*
   * NOTE: THIS IS FOR BACKWARD COMPATABILITY REASONS
   * Predicate returning @c true if this
   * object's "id" attribute is set.
   *
   * Most (but not all) objects in SBML include two common attributes: "id"
   * and "name".  The identifier given by an object's "id" attribute value
   * is used to identify the object within the SBML model definition.
   * Other objects can refer to the component using this identifier.  The
   * data type of "id" is always either <code>Sid</code> or
   * <code>UnitSId</code>, depending on the object in question.  Both
   * data types are defined as follows:
   * @code
   *   letter ::= 'a'..'z','A'..'Z'
   *   digit  ::= '0'..'9'
   *   idChar ::= letter | digit | '_'
   *   SId    ::= ( letter | '_' ) idChar*
   * @endcode
   *
   * The equality of <code>SId</code> and <code>UnitSId</code> type values
   * in SBML is determined by an exact character sequence match; i.e.,
   * comparisons of these identifiers must be performed in a case-sensitive
   * manner.  This applies to all uses of <code>SId</code> and
   * <code>UnitSId</code>.
   * 
   * @return @c true if the "id" attribute of this SBML object is
   * set, @c false otherwise.
   * 
   * @note The fact that the value of attribute "id" is defined on the
   * SBase parent class object is a convenience provided by libSBML, and
   * <b>does not strictly follow SBML specifications</b>.  This libSBML
   * implementation of SBase allows client applications to use more
   * generalized code in some situations (for instance, when manipulating
   * objects that are all known to have identifiers), but beware that not
   * all SBML object classes provide an "id" attribute.  LibSBML will allow
   * the identifier to be set, but it will not read nor write "id"
   * attributes for objects that do not possess them according to the SBML
   * specification for the Level and Version in use.
   *
   * @see getId()
   * @see setId(const std::string& sid)
   * @see unsetId()
   */
  bool isSetId () const;
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /*
   * NOTE: THIS IS FOR BACKWARD COMPATABILITY REASONS
   * Predicate returning @c true if this
   * object's "name" attribute is set.
   * 
   * Most (but not all) objects in SBML include two common attributes: "id"
   * and "name".  In contrast to the "id" attribute, the "name" attribute is
   * optional and is not intended to be used for cross-referencing purposes
   * within a model.  Its purpose instead is to provide a human-readable
   * label for the component.  The data type of "name" is the type
   * <code>string</code> defined in XML Schema.  SBML imposes no
   * restrictions as to the content of "name" attributes beyond those
   * restrictions defined by the <code>string</code> type in XML Schema.
   * 
   * The recommended practice for handling "name" is as follows.  If a
   * software tool has the capability for displaying the content of "name"
   * attributes, it should display this content to the user as a
   * component's label instead of the component's "id".  If the user
   * interface does not have this capability (e.g., because it cannot
   * display or use special characters in symbol names), or if the "name"
   * attribute is missing on a given component, then the user interface
   * should display the value of the "id" attribute instead.  (Script
   * language interpreters are especially likely to display "id" instead of
   * "name".)
   * 
   * As a consequence of the above, authors of systems that automatically
   * generate the values of "id" attributes should be aware some systems
   * may display the "id"'s to the user.  Authors therefore may wish to
   * take some care to have their software create "id" values that are: (a)
   * reasonably easy for humans to type and read; and (b) likely to be
   * meaningful, for example by making the "id" attribute be an abbreviated
   * form of the name attribute value.
   * 
   * An additional point worth mentioning is although there are
   * restrictions on the uniqueness of "id" values, there are no
   * restrictions on the uniqueness of "name" values in a model.  This
   * allows software packages leeway in assigning component identifiers.
   *
   * @return @c true if the "name" attribute of this SBML object is
   * set, @c false otherwise.
   *
   * @note The fact that the "name" attribute is defined on the SBase parent
   * class object is a convenience provided by libSBML, and <b>does not
   * strictly follow SBML specifications</b>.  This libSBML implementation
   * of SBase allows client applications to use more generalized code in
   * some situations (for instance, when manipulating objects that are all
   * known to have identifiers), but beware that not all SBML object
   * classes provide an "id" attribute.  LibSBML will allow the identifier
   * to be set, but it will not read nor write "id" attributes for objects
   * that do not possess them according to the SBML specification for the
   * Level and Version in use.
   *
   * @see getName()
   * @see setName(const std::string& name)
   * @see unsetName()
   */
  bool isSetName () const;
  /** @endcond */


  /**
   * Predicate returning @c true if this
   * object's "notes" subelement exists and has content.
   *
   * The optional SBML element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments about the
   * model element in which the "notes" element is enclosed.  Every object
   * derived directly or indirectly from type SBase can have a separate
   * value for "notes", allowing users considerable freedom when adding
   * comments to their models.
   *
   * The format of "notes" elements must be <a target="_blank"
   * href="http://www.w3.org/TR/xhtml1/">XHTML&nbsp;1.0</a>.  To help
   * verify the formatting of "notes" content, libSBML provides the static
   * utility method SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif); however,
   * readers are urged to consult the appropriate <a target="_blank"
   * href="http://sbml.org/Documents/Specifications">SBML specification
   * document</a> for the Level and Version of their model for more
   * in-depth explanations.  The SBML Level&nbsp;2 and &nbsp;3
   * specifications have considerable detail about how "notes" element
   * content must be structured.
   *
   * @return @c true if a "notes" subelement exists, @c false otherwise.
   * 
   * @see getNotes()
   * @see getNotesString()
   * @see setNotes(const XMLNode* notes)
   * @see setNotes(const std::string& notes)
   * @see appendNotes(const XMLNode* notes)
   * @see appendNotes(const std::string& notes)
   * @see unsetNotes()
   * @see SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif)
   */
  bool isSetNotes () const;


  /**
   * Predicate returning @c true if this
   * object's "annotation" subelement exists and has content.
   *
   * Whereas the SBase "notes" subelement is a container for content to be
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
   * @return @c true if a "annotation" subelement exists, @c false
   * otherwise.
   * 
   * @see getAnnotation()
   * @see getAnnotationString()
   * @see setAnnotation(const XMLNode* annotation)
   * @see setAnnotation(const std::string& annotation)
   * @see appendAnnotation(const XMLNode* annotation)
   * @see appendAnnotation(const std::string& annotation)
   * @see unsetAnnotation()
   */
  bool isSetAnnotation () const;


  /**
   * Predicate returning @c true if this
   * object's "sboTerm" attribute is set.
   *
   * @return @c true if the "sboTerm" attribute of this SBML object is
   * set, @c false otherwise.
   */
  bool isSetSBOTerm () const;


  /**
   * Sets the value of the "metaid" attribute of this object.
   *
   * The string @p metaid is copied.  The value of @p metaid must be an
   * identifier conforming to the syntax defined by the XML 1.0 data type
   * <a href="http://www.w3.org/TR/REC-xml/#id">ID</a>.  Among other
   * things, this type requires that a value is unique among all the values
   * of type XML ID in an SBMLDocument.  Although SBML only uses <a
   * href="http://www.w3.org/TR/REC-xml/#id">XML ID</a> for the "metaid"
   * attribute, callers should be careful if they use
   * <a href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>'s in XML
   * portions of a model that are not defined by SBML, such as in the
   * application-specific content of the "annotation" subelement.
   *
   * @param metaid the identifier string to use as the value of the
   * "metaid" attribute
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   * 
   * @see getMetaId()
   * @see isSetMetaId()
   */
  int setMetaId (const std::string& metaid);


  /**
   * Predicate returning @c true if this
   * object has a ModelHistory object attached to it.
   *
   * @return @c true if the ModelHistory of this object is set, @c
   * false otherwise.
   * 
   * @note In SBML Level&nbsp;2, model history annotations were only
   * permitted on the Model element.  In SBML Level&nbsp;3, they are
   * permitted on all SBML components derived from SBase.
   */
  bool isSetModelHistory();


  /** @cond doxygen-libsbml-internal */
  /*
   * NOTE: THIS IS FOR BACKWARD COMPATABILITY REASONS
   *
   * Sets the value of the "id" attribute of this SBML object to a copy
   * of @p id.
   *
   * The string @p sid is copied.  Note that SBML has strict requirements
   * for the syntax of identifiers.  @htmlinclude id-syntax.html
   *
   * @param sid the string to use as the identifier of this object
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
   int setId (const std::string& sid);
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /*
   * NOTE: THIS IS FOR BACKWARD COMPATABILITY REASONS
   * Sets the value of the "name" attribute of this SBML object to a copy
   * of @p name.
   * 
   * Most (but not all) objects in SBML include two common attributes: "id"
   * and "name".  In contrast to the "id" attribute, the "name" attribute is
   * optional and is not intended to be used for cross-referencing purposes
   * within a model.  Its purpose instead is to provide a human-readable
   * label for the component.  The data type of "name" is the type
   * <code>string</code> defined in XML Schema.  SBML imposes no
   * restrictions as to the content of "name" attributes beyond those
   * restrictions defined by the <code>string</code> type in XML Schema.
   *
   * The recommended practice for handling "name" is as follows.  If a
   * software tool has the capability for displaying the content of "name"
   * attributes, it should display this content to the user as a
   * component's label instead of the component's "id".  If the user
   * interface does not have this capability (e.g., because it cannot
   * display or use special characters in symbol names), or if the "name"
   * attribute is missing on a given component, then the user interface
   * should display the value of the "id" attribute instead.  (Script
   * language interpreters are especially likely to display "id" instead of
   * "name".)
   * 
   * As a consequence of the above, authors of systems that automatically
   * generate the values of "id" attributes should be aware some systems
   * may display the "id"'s to the user.  Authors therefore may wish to
   * take some care to have their software create "id" values that are: (a)
   * reasonably easy for humans to type and read; and (b) likely to be
   * meaningful, for example by making the "id" attribute be an abbreviated
   * form of the name attribute value.
   * 
   * An additional point worth mentioning is although there are
   * restrictions on the uniqueness of "id" values, there are no
   * restrictions on the uniqueness of "name" values in a model.  This
   * allows software packages leeway in assigning component identifiers.
   * 
   * @param name the new name for the object; the string will be copied
   *
   * @note The fact that the "name" attribute is defined on the SBase parent
   * class object is a convenience provided by libSBML, and <b>does not
   * strictly follow SBML specifications</b>.  This libSBML implementation
   * of SBase allows client applications to use more generalized code in
   * some situations (for instance, when manipulating objects that are all
   * known to have identifiers), but beware that not all SBML object
   * classes provide an "id" attribute.  LibSBML will allow the identifier
   * to be set, but it will not read nor write "id" attributes for objects
   * that do not possess them according to the SBML specification for the
   * Level and Version in use.
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   *
   * @see getName()
   * @see isSetName()
   * @see unsetName()
   */
  int setName (const std::string& name);
  /** @endcond */


  /**
   * Sets the value of the "annotation" subelement of this SBML object.
   *
   * The content of @p annotation is copied, and any previous content of
   * this object's "annotation" subelement is deleted.
   * 
   * Whereas the SBase "notes" subelement is a container for content to be
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
   * Call this method will result in any existing content of the
   * "annotation" subelement to be discarded.  Unless you have taken steps
   * to first copy and reconstitute any existing annotations into the @p
   * annotation that is about to be assigned, it is likely that performing
   * such wholesale replacement is unfriendly towards other software
   * applications whose annotations are discarded.  An alternative may be
   * to use SBase::appendAnnotation(const XMLNode* annotation) or
   * SBase::appendAnnotation(const std::string& annotation).
   *
   * @param annotation an XML structure that is to be used as the new content
   * of the "annotation" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   *
   * @see getAnnotationString()
   * @see isSetAnnotation()
   * @see setAnnotation(const std::string& annotation)
   * @see appendAnnotation(const XMLNode* annotation)
   * @see appendAnnotation(const std::string& annotation)
   * @see unsetAnnotation()
   */
  virtual int setAnnotation (const XMLNode* annotation);


  /**
   * Sets the value of the "annotation" subelement of this SBML object.
   *
   * The content of @p annotation is copied, and any previous content of
   * this object's "annotation" subelement is deleted.
   * 
   * Whereas the SBase "notes" subelement is a container for content to be
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
   * Call this method will result in any existing content of the
   * "annotation" subelement to be discarded.  Unless you have taken steps
   * to first copy and reconstitute any existing annotations into the @p
   * annotation that is about to be assigned, it is likely that performing
   * such wholesale replacement is unfriendly towards other software
   * applications whose annotations are discarded.  An alternative may be
   * to use SBase::appendAnnotation(const XMLNode* annotation) or
   * SBase::appendAnnotation(const std::string& annotation).
   *
   * @param annotation an XML string that is to be used as the content
   * of the "annotation" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   *
   * @see getAnnotationString()
   * @see isSetAnnotation()
   * @see setAnnotation(const XMLNode* annotation)
   * @see appendAnnotation(const XMLNode* annotation)
   * @see appendAnnotation(const std::string& annotation)
   * @see unsetAnnotation()
   */
  virtual int setAnnotation (const std::string& annotation);


  /**
   * Appends the given @p annotation to the "annotation" subelement of this
   * object.
   * 
   * Whereas the SBase "notes" subelement is a container for content to be
   * shown directly to humans, the "annotation" element is a container for
   * optional software-generated content @em not meant to be shown to
   * humans.  Every object derived from SBase can have its own value for
   * "annotation".  The element's content type is <a
   * target="_blank"
   * href="http://www.w3.org/TR/2004/REC-xml-20040204/#elemdecls">XML type "any"</a>,
   * allowing essentially arbitrary well-formed XML data content.
   *
   * SBML places a few restrictions on the organization of the content of
   * annotations; these are intended to help software tools read and write
   * the data as well as help reduce conflicts between annotations added by
   * different tools.  Please see the SBML specifications for more details.
   *
   * Unlike SBase::setAnnotation(const XMLNode* annotation) or
   * SBase::setAnnotation(const std::string& annotation), this method
   * allows other annotations to be preserved when an application adds its
   * own data.
   *
   * @param annotation an XML structure that is to be copied and appended
   * to the content of the "annotation" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   *
   * @see getAnnotationString()
   * @see isSetAnnotation()
   * @see setAnnotation(const XMLNode* annotation)
   * @see setAnnotation(const std::string& annotation)
   * @see appendAnnotation(const std::string& annotation)
   * @see unsetAnnotation()
   */
  virtual int appendAnnotation (const XMLNode* annotation);


  /**
   * Appends the given @p annotation to the "annotation" subelement of this
   * object.
   *
   * Whereas the SBase "notes" subelement is a container for content to be
   * shown directly to humans, the "annotation" element is a container for
   * optional software-generated content @em not meant to be shown to
   * humans.  Every object derived from SBase can have its own value for
   * "annotation".  The element's content type is <a
   * target="_blank"
   * href="http://www.w3.org/TR/2004/REC-xml-20040204/#elemdecls">XML type "any"</a>,
   * allowing essentially arbitrary well-formed XML data content.
   *
   * SBML places a few restrictions on the organization of the content of
   * annotations; these are intended to help software tools read and write
   * the data as well as help reduce conflicts between annotations added by
   * different tools.  Please see the SBML specifications for more details.
   *
   * Unlike SBase::setAnnotation(const XMLNode* annotation) or
   * SBase::setAnnotation(const std::string& annotation), this method
   * allows other annotations to be preserved when an application adds its
   * own data.
   *
   * @param annotation an XML string that is to be copied and appended
   * to the content of the "annotation" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   *
   * @see getAnnotationString()
   * @see isSetAnnotation()
   * @see setAnnotation(const XMLNode* annotation)
   * @see setAnnotation(const std::string& annotation)
   * @see appendAnnotation(const XMLNode* annotation)
   * @see unsetAnnotation()
   */
  virtual int appendAnnotation (const std::string& annotation);


  /**
   * Sets the value of the "notes" subelement of this SBML object.
   *
   * The content of @p notes is copied, and any existing content of this
   * object's "notes" subelement is deleted.
   *
   * The optional SBML element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments about the
   * model element in which the "notes" element is enclosed.  Every object
   * derived directly or indirectly from type SBase can have a separate
   * value for "notes", allowing users considerable freedom when adding
   * comments to their models.
   *
   * The format of "notes" elements must be <a target="_blank"
   * href="http://www.w3.org/TR/xhtml1/">XHTML&nbsp;1.0</a>.  To help
   * verify the formatting of "notes" content, libSBML provides the static
   * utility method SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif); however,
   * readers are urged to consult the appropriate <a target="_blank"
   * href="http://sbml.org/Documents/Specifications">SBML specification
   * document</a> for the Level and Version of their model for more
   * in-depth explanations.  The SBML Level&nbsp;2 and &nbsp;3
   * specifications have considerable detail about how "notes" element
   * content must be structured.
   *
   * @param notes an XML structure that is to be used as the content of the
   * "notes" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   *
   * @see getNotesString()
   * @see isSetNotes()
   * @see setNotes(const std::string& notes)
   * @see appendNotes(const XMLNode* notes)
   * @see appendNotes(const std::string& notes)
   * @see unsetNotes()
   * @see SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif)
   */
  int setNotes(const XMLNode* notes);


  /**
   * Sets the value of the "notes" subelement of this SBML object to a copy
   * of the string @p notes.
   *
   * The content of @p notes is copied, and any existing content of this
   * object's "notes" subelement is deleted.
   *
   * The optional SBML element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments about the
   * model element in which the "notes" element is enclosed.  Every object
   * derived directly or indirectly from type SBase can have a separate
   * value for "notes", allowing users considerable freedom when adding
   * comments to their models.
   *
   * The format of "notes" elements must be <a target="_blank"
   * href="http://www.w3.org/TR/xhtml1/">XHTML&nbsp;1.0</a>.  To help
   * verify the formatting of "notes" content, libSBML provides the static
   * utility method SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif); however,
   * readers are urged to consult the appropriate <a target="_blank"
   * href="http://sbml.org/Documents/Specifications">SBML specification
   * document</a> for the Level and Version of their model for more
   * in-depth explanations.  The SBML Level&nbsp;2 and &nbsp;3
   * specifications have considerable detail about how "notes" element
   * content must be structured.
   *
   * @param notes an XML string that is to be used as the content of the
   * "notes" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   *
   * @see getNotesString()
   * @see isSetNotes()
   * @see setNotes(const XMLNode* notes)
   * @see appendNotes(const XMLNode* notes)
   * @see appendNotes(const std::string& notes)
   * @see unsetNotes()
   * @see SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif)
   */
  int setNotes(const std::string& notes);


  /**
   * Appends the given @p notes to the "notes" subelement of this object.
   *
   * The content of @p notes is copied.
   *
   * The optional SBML element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments about the
   * model element in which the "notes" element is enclosed.  Every object
   * derived directly or indirectly from type SBase can have a separate
   * value for "notes", allowing users considerable freedom when adding
   * comments to their models.
   *
   * The format of "notes" elements must be <a target="_blank"
   * href="http://www.w3.org/TR/xhtml1/">XHTML&nbsp;1.0</a>.  To help
   * verify the formatting of "notes" content, libSBML provides the static
   * utility method SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif); however,
   * readers are urged to consult the appropriate <a target="_blank"
   * href="http://sbml.org/Documents/Specifications">SBML specification
   * document</a> for the Level and Version of their model for more
   * in-depth explanations.  The SBML Level&nbsp;2 and &nbsp;3
   * specifications have considerable detail about how "notes" element
   * content must be structured.
   * 
   * @param notes an XML node structure that is to appended to the content
   * of the "notes" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   *
   * @see getNotesString()
   * @see isSetNotes()
   * @see setNotes(const XMLNode* notes)
   * @see setNotes(const std::string& notes)
   * @see appendNotes(const std::string& notes)
   * @see unsetNotes()
   * @see SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif)
   */
  int appendNotes(const XMLNode* notes);


  /**
   * Appends the given @p notes to the "notes" subelement of this object.
   *
   * The content of the parameter @p notes is copied.
   *
   * The optional SBML element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments about the
   * model element in which the "notes" element is enclosed.  Every object
   * derived directly or indirectly from type SBase can have a separate
   * value for "notes", allowing users considerable freedom when adding
   * comments to their models.
   *
   * The format of "notes" elements must be <a target="_blank"
   * href="http://www.w3.org/TR/xhtml1/">XHTML&nbsp;1.0</a>.  To help
   * verify the formatting of "notes" content, libSBML provides the static
   * utility method SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif); however,
   * readers are urged to consult the appropriate <a target="_blank"
   * href="http://sbml.org/Documents/Specifications">SBML specification
   * document</a> for the Level and Version of their model for more
   * in-depth explanations.  The SBML Level&nbsp;2 and &nbsp;3
   * specifications have considerable detail about how "notes" element
   * content must be structured.
   *
   * @param notes an XML string that is to appended to the content of
   * the "notes" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   *
   * @see getNotesString()
   * @see isSetNotes()
   * @see setNotes(const XMLNode* notes)
   * @see setNotes(const std::string& notes)
   * @see appendNotes(const XMLNode* notes)
   * @see unsetNotes()
   * @see SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif)
   */
  int appendNotes(const std::string& notes);


  /**
   * Sets the ModelHistory of this object.
   *
   * The content of @p history is copied, and this object's existing model
   * history content is deleted.
   *
   * @param history ModelHistory of this object.
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   * 
   * @note In SBML Level&nbsp;2, model history annotations were only
   * permitted on the Model element.  In SBML Level&nbsp;3, they are
   * permitted on all SBML components derived from SBase.
   */
  int setModelHistory(ModelHistory * history);


  /** @cond doxygen-libsbml-internal */
  /**
   * Sets the parent SBMLDocument of this SBML object.
   *
   * @param d the SBMLDocument object to use
   */
  virtual void setSBMLDocument (SBMLDocument* d);
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /**
   * Sets the parent SBML object of this SBML object.
   *
   * @param sb the SBML object to use
   */
  virtual void setParentSBMLObject (SBase* sb);
  /** @endcond */


  /**
   * Sets the value of the "sboTerm" attribute.
   *
   * Beginning with SBML Level 2 Version 3, objects derived from SBase have
   * an optional attribute named "sboTerm" for supporting the use of the
   * Systems Biology Ontology.  In SBML proper, the data type of the
   * attribute is a string of the form "SBO:NNNNNNN", where "NNNNNNN" is a
   * seven digit integer number; libSBML simplifies the representation by
   * only storing the "NNNNNNN" integer portion.  Thus, in libSBML, the
   * "sboTerm" attribute on SBase has data type @c int, and SBO identifiers
   * are stored simply as integers. 
   *
   * SBO terms are a type of optional annotation, and each different class
   * of SBML object derived from SBase imposes its own requirements about
   * the values permitted for "sboTerm".  Please consult the SBML
   * Level&nbsp;2 Version&nbsp;4 specification for more information about
   * the use of SBO and the "sboTerm" attribute.
   *
   * @param value the NNNNNNN integer portion of the SBO identifier
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   *
   * @see setSBOTerm(@if clike const std::string &sboid@endif@if java String sbo_id@endif)
   */
  virtual int setSBOTerm (int value);


  /**
   * Sets the value of the "sboTerm" attribute by string.
   *
   * Beginning with SBML Level 2 Version 3, objects derived from SBase have
   * an optional attribute named "sboTerm" for supporting the use of the
   * Systems Biology Ontology.  In SBML proper, the data type of the
   * attribute is a string of the form "SBO:NNNNNNN", where "NNNNNNN" is a
   * seven digit integer number; libSBML simplifies the representation by
   * only storing the "NNNNNNN" integer portion.  Thus, in libSBML, the
   * "sboTerm" attribute on SBase has data type @c int, and SBO identifiers
   * are stored simply as integers.  This method lets you set the value of
   * "sboTerm" as a complete string of the form "SBO:NNNNNNN", whereas
   * setSBOTerm(int value) allows you to set it using the integer form.
   *
   * SBO terms are a type of optional annotation, and each different class
   * of SBML object derived from SBase imposes its own requirements about
   * the values permitted for "sboTerm".  Please consult the SBML
   * Level&nbsp;2 Version&nbsp;4 specification for more information about
   * the use of SBO and the "sboTerm" attribute.
   *
   * @param sboid the SBO identifier string of the form "SBO:NNNNNNN"
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   *
   * @see setSBOTerm(int value)
   */
  virtual int setSBOTerm (const std::string &sboid);


  /**
   * Sets the namespaces relevant of this SBML object.
   *
   * The content of @p xmlns is copied, and this object's existing
   * namespace content is deleted.
   *
   * The SBMLNamespaces object encapsulates SBML Level/Version/namespaces
   * information.  It is used to communicate the SBML Level, Version, and
   * (in Level&nbsp;3) packages used in addition to SBML Level&nbsp;3 Core.
   * 
   * @param xmlns the namespaces to set
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   */
  int setNamespaces(XMLNamespaces* xmlns);


  /**
   * Unsets the value of the "metaid" attribute of this SBML object.
   *
   * The optional attribute named "metaid", present on every major SBML
   * component type, is for supporting metadata annotations using RDF
   * (Resource Description Format). The attribute value has the data type
   * <a href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>, the XML
   * identifier type, which means each "metaid" value must be globally
   * unique within an SBML file.  (Importantly, this uniqueness criterion
   * applies across any attribute with type <a
   * href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>, not just the
   * "metaid" attribute used by SBML&mdash;something to be aware of if your
   * application-specific XML content inside the "annotation" subelement
   * happens to use <a href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>.)
   * The "metaid" value serves to identify a model component for purposes
   * such as referencing that component from metadata placed within
   * "annotation" subelements.
   *  
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  int unsetMetaId ();


#if 0
  /**
   * Unsets the value of the "id" attribute of this SBML object.
   *
   * Most (but not all) objects in SBML include two common attributes: "id"
   * and "name".  The identifier given by an object's "id" attribute value
   * is used to identify the object within the SBML model definition.
   * Other objects can refer to the component using this identifier.  The
   * data type of "id" is always either <code>Sid</code> or
   * <code>UnitSId</code>, depending on the object in question.  Both
   * data types are defined as follows:
   * @code
   *   letter ::= 'a'..'z','A'..'Z'
   *   digit  ::= '0'..'9'
   *   idChar ::= letter | digit | '_'
   *   SId    ::= ( letter | '_' ) idChar*
   * @endcode
   *
   * The equality of <code>SId</code> and <code>UnitSId</code> type values
   * in SBML is determined by an exact character sequence match; i.e.,
   * comparisons of these identifiers must be performed in a case-sensitive
   * manner.  This applies to all uses of <code>SId</code> and
   * <code>UnitSId</code>.
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   *
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  int unsetId ();


  /**
   * Unsets the value of the "name" attribute of this SBML object.
   * 
   * Most (but not all) objects in SBML include two common attributes: "id"
   * and "name".  In contrast to the "id" attribute, the "name" attribute is
   * optional and is not intended to be used for cross-referencing purposes
   * within a model.  Its purpose instead is to provide a human-readable
   * label for the component.  The data type of "name" is the type
   * <code>string</code> defined in XML Schema.  SBML imposes no
   * restrictions as to the content of "name" attributes beyond those
   * restrictions defined by the <code>string</code> type in XML Schema.
   *
   * The recommended practice for handling "name" is as follows.  If a
   * software tool has the capability for displaying the content of "name"
   * attributes, it should display this content to the user as a
   * component's label instead of the component's "id".  If the user
   * interface does not have this capability (e.g., because it cannot
   * display or use special characters in symbol names), or if the "name"
   * attribute is missing on a given component, then the user interface
   * should display the value of the "id" attribute instead.  (Script
   * language interpreters are especially likely to display "id" instead of
   * "name".)
   * 
   * As a consequence of the above, authors of systems that automatically
   * generate the values of "id" attributes should be aware some systems
   * may display the "id"'s to the user.  Authors therefore may wish to
   * take some care to have their software create "id" values that are: (a)
   * reasonably easy for humans to type and read; and (b) likely to be
   * meaningful, for example by making the "id" attribute be an abbreviated
   * form of the name attribute value.
   * 
   * An additional point worth mentioning is although there are
   * restrictions on the uniqueness of "id" values, there are no
   * restrictions on the uniqueness of "name" values in a model.  This
   * allows software packages leeway in assigning component identifiers.
   * 
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   *
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  int unsetName ();
#endif


  /**
   * Unsets the value of the "notes" subelement of this SBML object.
   *
   * The optional SBML element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments about the
   * model element in which the "notes" element is enclosed.  Every object
   * derived directly or indirectly from type SBase can have a separate
   * value for "notes", allowing users considerable freedom when adding
   * comments to their models.
   *
   * The format of "notes" elements must be <a target="_blank"
   * href="http://www.w3.org/TR/xhtml1/">XHTML&nbsp;1.0</a>.  To help
   * verify the formatting of "notes" content, libSBML provides the static
   * utility method SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif); however,
   * readers are urged to consult the appropriate <a target="_blank"
   * href="http://sbml.org/Documents/Specifications">SBML specification
   * document</a> for the Level and Version of their model for more
   * in-depth explanations.  The SBML Level&nbsp;2 and &nbsp;3
   * specifications have considerable detail about how "notes" element
   * content must be structured.
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   *
   * @see getNotesString()
   * @see isSetNotes()
   * @see setNotes(const XMLNode* notes)
   * @see setNotes(const std::string& notes)
   * @see appendNotes(const XMLNode* notes)
   * @see appendNotes(const std::string& notes)
   * @see SyntaxChecker::hasExpectedXHTMLSyntax(@if java XMLNode xhtml@endif)
   */
  int unsetNotes ();


  /**
   * Unsets the value of the "annotation" subelement of this SBML object.
   *
   * Whereas the SBase "notes" subelement is a container for content to be
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
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   *
   * @see getAnnotation()
   * @see getAnnotationString()
   * @see isSetAnnotation()
   * @see setAnnotation(const XMLNode* annotation)
   * @see setAnnotation(const std::string& annotation)
   * @see appendAnnotation(const XMLNode* annotation)
   * @see appendAnnotation(const std::string& annotation)
   */
  int unsetAnnotation ();


  /**
   * Unsets the value of the "sboTerm" attribute of this SBML object.
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   */
  int unsetSBOTerm ();


  /**
   * Adds a copy of the given CVTerm object to this SBML object.
   *
   * @param term the CVTerm to assign
   * 
   * @param newBag if @c true, creates a new RDF bag with the same identifier
   * as a previous bag, and if @c false, adds the term to an existing
   * RDF bag with the same type of qualifier as the term being added.
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink, if
   * this object lacks a "metaid" attribute
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   *
   * @note Since the CV Term uses the "metaid" attribute of the object as a
   * reference, if the object has no "metaid" attribute value set, then the
   * CVTerm will not be added.
   *
   * @warning The fact that this method @em copies the object passed to it
   * means that the caller will be left holding a physically different
   * object instance than the one contained in @em this object.  Changes
   * made to the original object instance (such as resetting attribute
   * values) will <em>not affect the instance added here</em>.  In
   * addition, the caller should make sure to free the original object if
   * it is no longer being used, or else a memory leak will result.
   */
  int addCVTerm(CVTerm * term, bool newBag = false);


  /**
   * Returns a list of CVTerm objects in the annotations of this SBML
   * object.
   * 
   * @return the list of CVTerms for this SBML object.
   */
  List* getCVTerms();


  /**
   * Returns a list of CVTerm objects in the annotations of this SBML
   * object.
   * 
   * @return the list of CVTerms for this SBML object.
   */
  List* getCVTerms()  const;


  /**
   * Returns the number of CVTerm objects in the annotations of this SBML
   * object.
   * 
   * @return the number of CVTerms for this SBML object.
   */
  unsigned int getNumCVTerms();


  /**
   * Returns the nth CVTerm in the list of CVTerms of this SBML
   * object.
   * 
   * @param n unsigned int the index of the CVTerm to retrieve
   *
   * @return the nth CVTerm in the list of CVTerms for this SBML object.
   */
  CVTerm* getCVTerm(unsigned int n);


  /**
   * Clears the list of CVTerm objects attached to this SBML object.
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  int unsetCVTerms();


  /**
   * Unsets the ModelHistory object attached to this object.
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   * 
   * @note In SBML Level&nbsp;2, model history annotations were only
   * permitted on the Model element.  In SBML Level&nbsp;3, they are
   * permitted on all SBML components derived from SBase.
   */
  int unsetModelHistory();


  /**
   * Returns the MIRIAM <em>biological qualifier</em> associated with the
   * given resource.
   *
   * In <a target="_blank" href="http://biomodels.net/miriam">MIRIAM</a>,
   * qualifiers are an optional means of indicating the relationship
   * between a model component and its annotations.  There are two broad
   * kinds of annotations: <em>model</em> and <em>biological</em>.  The
   * latter kind is used to qualify the relationship between a model
   * component and a biological entity which it represents.  Examples of
   * relationships include "is" and "has part", but many others are
   * possible.  MIRIAM defines <a target="_blank"
   * href="http://www.ebi.ac.uk/miriam/main/qualifiers/">numerous
   * relationship qualifiers</a> to enable different software tools to
   * qualify biological annotations in the same standardized way.  In
   * libSBML, the MIRIAM controlled-vocabulary annotations on an SBML model
   * element are represented using lists of CVTerm objects, and the
   * the MIRIAM biological qualifiers are represented using
   * values @if clike from the enumeration
   * type #BiolQualifierType_t.@endif@if python whose
   * names begin with <code>BQB_</code> in the interface class
   * @link libsbml libsbml@endlink.@endif@if java whose
   * names begin with <code>BQB_</code> in the interface class
   * {@link libsbmlConstants}.@endif
   *
   * This method searches the controlled-vocabulary annotations
   * (i.e., the list of CVTerm objects) on the present object, then out of
   * those that have biological qualifiers, looks for an annotation to the
   * given @p resource.  If such an annotation is found, it returns the
   * type of biological qualifier associated with that resource as a 
   * value @if clike from the enumeration type
   * #BiolQualifierType_t.@endif@if python whose name begins with
   * <code>BQB_</code> from the interface
   * class @link libsbml libsbml@endlink.@endif@if java whose name
   * begins with <code>BQB_</code> from the interface
   * class {@link libsbmlConstants}.@endif
   *
   * @param resource string representing the resource; e.g.,
   * <code>"http://www.geneontology.org/#GO:0005892"</code>.
   *
   * @return the qualifier associated with the resource,
   * or @link BiolQualifierType_t#BQB_UNKNOWN BQB_UNKNOWN@endlink if the
   * resource does not exist.
   *
   * @if clike
   * @note The set of MIRIAM biological qualifiers grows over
   * time, although relatively slowly.  The values in the enumeration
   * #BiolQualifierType_t are up to date with MIRIAM at the time of a given
   * libSBML release.  The set of values may be expanded in later libSBML
   * releases, to match the values defined by MIRIAM at that later time.
   * @endif@if python
   * @note The set of MIRIAM biological qualifiers grows over
   * time, although relatively slowly.  The values are up to date with
   * MIRIAM at the time of a given libSBML release.  The set of values in
   * list of <code>BQB_</code> constants defined in @link libsbml
   * libsbml@endlink may be expanded in later libSBML releases, to match
   * the values defined by MIRIAM at that later time.
   * @endif@if java
   * @note The set of MIRIAM biological qualifiers grows over
   * time, although relatively slowly.  The values are up to date with
   * MIRIAM at the time of a given libSBML release.  The set of values in
   * list of <code>BQB_</code> constants defined in {@link libsbmlConstants}
   * may be expanded in later libSBML releases, to match
   * the values defined by MIRIAM at that later time.
   * @endif
   */
  BiolQualifierType_t getResourceBiologicalQualifier(std::string resource);


  /**
   * Returns the MIRIAM <em>model qualifier</em> associated with the
   * given resource.
   *
   * In <a target="_blank" href="http://biomodels.net/miriam">MIRIAM</a>,
   * qualifiers are an optional means of indicating the relationship
   * between a model component and its annotations.  There are two broad
   * kinds of annotations: <em>model</em> and <em>biological</em>.  The
   * former kind is used to qualify the relationship between a model
   * component and another modeling object.  An example qualifier is
   * "isDerivedFrom", to indicate that a given component of the model is
   * derived from the modeling object represented by the referenced
   * resource.  MIRIAM defines <a target="_blank"
   * href="http://www.ebi.ac.uk/miriam/main/qualifiers/">numerous
   * relationship qualifiers</a> to enable different software tools to
   * qualify model annotations in the same standardized way.  In libSBML,
   * the MIRIAM controlled-vocabulary annotations on an SBML model element
   * are represented using lists of CVTerm objects, and the 
   * the MIRIAM model qualifiers are represented using
   * values @if clike from the enumeration
   * type #ModelQualifierType_t.@endif@if python whose
   * names begin with <code>BQM_</code> in the interface class
   * @link libsbml libsbml@endlink.@endif@if java whose
   * names begin with <code>BQM_</code> in the interface class
   * {@link libsbmlConstants}.@endif
   *
   * This method method searches the controlled-vocabulary annotations
   * (i.e., the list of CVTerm objects) on the present object, then out of
   * those that have model qualifiers, looks for an annotation to the given
   * @p resource.  If such an annotation is found, it returns the type of
   * type of model qualifier associated with that resource as a 
   * value @if clike from the enumeration type
   * #ModelQualifierType_t.@endif@if python whose name begins with
   * <code>BQM_</code> from the interface
   * class @link libsbml libsbml@endlink.@endif@if java whose name
   * begins with <code>BQM_</code> from the interface
   * class {@link libsbmlConstants}.@endif
   *
   * @param resource string representing the resource; e.g.,
   * <code>"http://www.geneontology.org/#GO:0005892"</code>.
   *
   * @return the #ModelQualifierType_t value associated with the resource,
   * or @link ModelQualifierType_t#BQM_UNKNOWN BQM_UNKNOWN@endlink if the
   * resource does not exist.
   *
   * @if clike
   * @note The set of MIRIAM biological qualifiers grows over
   * time, although relatively slowly.  The values in the enumeration
   * #ModelQualifierType_t are up to date with MIRIAM at the time of a given
   * libSBML release.  The set of values may be expanded in later libSBML
   * releases, to match the values defined by MIRIAM at that later time.
   * @endif@if python
   * @note The set of MIRIAM model qualifiers grows over
   * time, although relatively slowly.  The values are up to date with
   * MIRIAM at the time of a given libSBML release.  The set of values in
   * list of <code>BQM_</code> constants defined in @link libsbml
   * libsbml@endlink may be expanded in later libSBML releases, to match
   * the values defined by MIRIAM at that later time.
   * @endif@if java
   * @note The set of MIRIAM model qualifiers grows over
   * time, although relatively slowly.  The values are up to date with
   * MIRIAM at the time of a given libSBML release.  The set of values in
   * list of <code>BQM_</code> constants defined in {@link libsbmlConstants}
   * may be expanded in later libSBML releases, to match
   * the values defined by MIRIAM at that later time.
   * @endif
   */
  ModelQualifierType_t getResourceModelQualifier(std::string resource);


  /**
   * Returns the Model object in which the current object is located.
   * 
   * @return the parent Model of this SBML object.
   *
   * @see getParentSBMLObject()
   * @see getSBMLDocument()
   */
  const Model* getModel () const;


  /**
   * Returns the SBML Level of the SBMLDocument object containing this
   * object.
   * 
   * @return the SBML level of this SBML object.
   * 
   * @see getVersion()
   * @see getNamespaces()
   */
  unsigned int getLevel () const;


  /**
   * Returns the Version within the SBML Level of the SBMLDocument object
   * containing this object.
   * 
   * @return the SBML version of this SBML object.
   *
   * @see getLevel()
   * @see getNamespaces()
   */
  unsigned int getVersion () const;


  /**
   * Returns the libSBML type code for this object.
   * 
   * This method may return the type code of this SBML object, or it may
   * return @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink.  This
   * is because subclasses of SBase are not required to implement this
   * method to return a type code.  This method is meant primarily for the
   * LibSBML C interface, in which class and subclass information is not
   * readily available.
   *
   * @return the @if clike #SBMLTypeCode_t value@endif@if notcpp SBML object type code@endif
   * of this SBML object or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (the default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;


  /**
   * Predicate returning @c true if this
   * object's level/version and namespace values correspond to a valid
   * SBML specification.
   *
   * The valid combinations of SBML Level, Version and Namespace as of this
   * release of libSBML are the following:
   * <ul>
   * <li> Level&nbsp;1 Version&nbsp;2: <code>"http://www.sbml.org/sbml/level1"</code>
   * <li> Level&nbsp;2 Version&nbsp;1: <code>"http://www.sbml.org/sbml/level2"</code>
   * <li> Level&nbsp;2 Version&nbsp;2: <code>"http://www.sbml.org/sbml/level2/version2"</code>
   * <li> Level&nbsp;2 Version&nbsp;3: <code>"http://www.sbml.org/sbml/level2/version3"</code>
   * <li> Level&nbsp;2 Version&nbsp;4: <code>"http://www.sbml.org/sbml/level2/version4"</code>
   * <li> Level&nbsp;3 Version&nbsp;1 Core: <code>"http://www.sbml.org/sbml/level3/version1/core"</code>
   * </ul>
   *
   * @return @c true if the level, version and namespace values of this 
   * SBML object correspond to a valid set of values, @c false otherwise.
   */
  bool hasValidLevelVersionNamespaceCombination();

  
  /**
   * Returns the XML element name of this object.
   *
   * This is overridden by subclasses to return a string appropriate to the
   * SBML component.  For example, Model defines it as returning @c
   * "model", CompartmentType defines it as returning @c "compartmentType",
   * and so on.
   */
  virtual const std::string& getElementName () const = 0;


  /**
   * Returns a string consisting of a partial SBML corresponding to just
   * this object.
   * 
   * @return the partial SBML that describes this SBML object.
   *
   * @warning This is primarily provided for testing and debugging
   * purposes.  It may be removed in a future version of libSBML.
   */
  char* toSBML ();


  /** @cond doxygen-libsbml-internal */
  /**
   * Reads (initializes) this SBML object by reading from XMLInputStream.
   */
  void read (XMLInputStream& stream);
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /**
   * Writes (serializes) this SBML object by writing it to XMLOutputStream.
   */
  void write (XMLOutputStream& stream) const;
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.  For example:@if clike
   * <pre>
   *   SBase::writeElements();
   *   mReactans.write(stream);
   *   mProducts.write(stream);
   *   ...
   * </pre>@endif
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /* function returns true if component has all the required
   * attributes
   * needs to be overloaded for each component
   */
  virtual bool hasRequiredAttributes() const ;
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /* function returns true if component has all the required
   * elements
   * needs to be overloaded for each component
   */
  virtual bool hasRequiredElements() const ;
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /* sets the SBMLnamespaces - internal use only*/
  void setSBMLNamespaces(SBMLNamespaces * sbmlns);
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /* gets the SBMLnamespaces - internal use only*/
  SBMLNamespaces * getSBMLNamespaces() const;
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /* removes duplicate top level annotations*/
  void removeDuplicateAnnotations();
  const std::string checkMathMLNamespace(const XMLToken elem);
  /** @endcond */


protected:
  /** @cond doxygen-libsbml-internal */


  /**
   * Only subclasses may create SBase objects.
   *
   * @if notcpp @docnote @htmlinclude warn-default-args-in-docs.html @endif
   */
  SBase (const std::string& id = "", const std::string& name = "", int sboTerm = -1);

  
  /**
   * Creates a new SBase object with the given sboTerm.
   * Only subclasses may create SBase objects.
   */
  SBase (unsigned int level, unsigned int version);


  /**
   * Creates a new SBase object with the given SBMLNamespaces.
   * Only subclasses may create SBase objects.
   */
  SBase (SBMLNamespaces* sbmlns);


  /**
  * Copy constructor. Creates a copy of this SBase object.
  */
  SBase(const SBase& orig);


  /**
   * Subclasses should override this method to create, store, and then
   * return an SBML object corresponding to the next XMLToken in the
   * XMLInputStream.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or @c NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);


  /**
   * Predicate returning @c true if this
   * object's level/version and namespace values correspond to a valid
   * SBML specification.
   *
   * The valid combinations of SBML Level, Version and Namespace as of this
   * release of libSBML are the following:
   * <ul>
   * <li> Level&nbsp;1 Version&nbsp;2: <code>"http://www.sbml.org/sbml/level1"</code>
   * <li> Level&nbsp;2 Version&nbsp;1: <code>"http://www.sbml.org/sbml/level2"</code>
   * <li> Level&nbsp;2 Version&nbsp;2: <code>"http://www.sbml.org/sbml/level2/version2"</code>
   * <li> Level&nbsp;2 Version&nbsp;3: <code>"http://www.sbml.org/sbml/level2/version3"</code>
   * <li> Level&nbsp;2 Version&nbsp;4: <code>"http://www.sbml.org/sbml/level2/version4"</code>
   * <li> Level&nbsp;3 Version&nbsp;1 Core: <code>"http://www.sbml.org/sbml/level3/version1/core"</code>
   * </ul>
   *
   * @param typecode the typecode for this element
   * @param xmlns the namespaces used by this element.
   *
   * @note  This function is provided as convenience method to be called from constructors. This 
   *        allows to use it in scenarios where the namespaces or typecode have not yet been initialized. 
   * 
   * @return @c true if the level, version and namespace values of this 
   * SBML object correspond to a valid set of values, @c false otherwise.
   */
  bool hasValidLevelVersionNamespaceCombination(int typecode, XMLNamespaces *xmlns);


  /**
   * Subclasses should override this method to read (and store) XHTML,
   * MathML, etc. directly from the XMLInputStream.
   *
   * @return true if the subclass read from the stream, false otherwise.
   */
  virtual bool readOtherXML (XMLInputStream& stream);


  /**
   * The SBML XML Schema is written such that the order of child elements
   * is significant.  LibSBML can read elements out of order.  If you
   * override this method to indicate the ordinal position of element with
   * respect to its siblings, libSBML will log an error if the element is
   * read out of order.
   *
   * @return the ordinal position of the element with respect to its
   * siblings or @c -1 (the default) to indicate the position is not
   * significant.
   */
  virtual int getElementPosition () const;


  /**
   * @return the SBMLErrorLog used to log errors during while reading and
   * validating SBML.
   */
  SBMLErrorLog* getErrorLog ();


  /**
   * Convenience method for easily logging problems from within method
   * implementations.
   *
   * This is essentially a short form of getErrorLog()->logError(...)
   *
   * @if notcpp @docnote @htmlinclude warn-default-args-in-docs.html @endif
   */
  void logError (  unsigned int       id
                 , const unsigned int level   = 2
                 , const unsigned int version = 3
                 , const std::string& details = "" );


  /**
   * Helper to log a common type of error.
   */
  void logUnknownAttribute( std::string attribute,
			    const unsigned int level,
			    const unsigned int version,
			    const std::string element );


  /**
   * Helper to log a common type of error.
   */
  void logUnknownElement( const std::string element,
			  const unsigned int level,
			  const unsigned int version );

 
  /**
   * Helper to log a common type of error.
   */
  void logEmptyString( std::string attribute,
			    const unsigned int level,
			    const unsigned int version,
          std::string element);


  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes);


  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.  For example:
   *
   *   SBase::writeAttributes(stream);
   *   stream.writeAttribute( "id"  , mId   );
   *   stream.writeAttribute( "name", mName );
   *   ...
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  /**
   * Synchronizes the annotation of this SBML object.
   *
   * Annotation element (XMLNode* mAnnotation) is synchronized with the 
   * current CVTerm objects (List* mCVTerm).
   * Currently, this method is called in getAnnotation, isSetAnnotation,
   * and writeElements methods.
   */
  virtual void syncAnnotation();


  /**
   * Checks that the SBML element appears in the expected order.
   *
   * If @p object is not in the expected position, an error is logged.
   */
  void checkOrderAndLogError (SBase* object, int expected);


  /**
   * Checks that an SBML ListOf element is populated.  
   * If a listOf element has been declared with no elements, 
   * an error is logged.
   */
  void checkListOfPopulated(SBase* object);


#if 0
  /**
   * Checks the syntax of the unit attribute.
   * The syntax of an unit is of type UnitSId which is defined as:
   *
   *  - letter ::= 'a'..'z','A'..'Z'
   *  - digit  ::= '0'..'9'
   *  - idChar ::= letter | digit | '_'
   *  - UnitSId    ::= ( letter | '_' ) idChar*
   *
   * If the syntax of the unit attribute of this object is incorrect, 
   * an error is logged
   *
   * @if notcpp @docnote @htmlinclude warn-default-args-in-docs.html @endif
   */
  void checkUnitSyntax(unsigned int flag = 0);
#endif

  /**
   * Checks that the given default namespace in the given element is valid.
   * If the given default namespace is not valid, an error is logged.
   */
  void checkDefaultNamespace(const XMLNamespaces* xmlns, 
    const std::string& elementName, const std::string& prefix = "");

  /**
   * Checks the annotation does not declare an sbml namespace.
   * If the annotation declares an sbml namespace an error is logged.
   */
  void checkAnnotation();


  /**
   * Checks that the XHTML is valid.
   * If the xhtml does not conform to the specification of valid xhtml within
   * an sbml document, an error is logged.
   */
  void checkXHTML(const XMLNode *);


  std::string mMetaId;
  //std::string mId;
  //std::string mName;

  XMLNode* mNotes;
  XMLNode* mAnnotation;

  //XMLNamespaces* mNamespaces;

  SBMLDocument* mSBML;

  //int mObjectLevel;
  //int mObjectVersion;

  SBMLNamespaces *mSBMLNamespaces;

  int mSBOTerm;

  unsigned int mLine;
  unsigned int mColumn;

  /* store the parent SBML object */
  SBase* mParentSBMLObject;

  /* storing annotations */
  List * mCVTerms;
  ModelHistory*   mHistory;

  /* flag that allows object to know its been deleted
   * for OS where the memory is still readable after a delete
   */
  bool mHasBeenDeleted;

  std::string mEmptyString;

  /** @endcond */


private:
  /** @cond doxygen-libsbml-internal */

  /**
   * Stores the location (line and column) and any XML namespaces (for
   * roundtripping) declared on this SBML (XML) element.
   */
  void setSBaseFields (const XMLToken& element);


  /**
   * @return true if read an <annotation> element from the stream
   */
  bool readAnnotation (XMLInputStream& stream);


  /**
   * @return true if read a <notes> element from the stream
   */
  bool readNotes (XMLInputStream& stream);

  bool getHasBeenDeleted();

  /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */



#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/


LIBSBML_EXTERN
const char *
SBase_getMetaId (SBase_t *sb);




LIBSBML_EXTERN
const SBMLDocument_t *
SBase_getSBMLDocument (SBase_t *sb);


LIBSBML_EXTERN
const SBase_t *
SBase_getParentSBMLObject (SBase_t *sb);


LIBSBML_EXTERN
const SBase_t *
SBase_getAncestorOfType (SBase_t *sb, SBMLTypeCode_t type);

LIBSBML_EXTERN
int
SBase_getSBOTerm (const SBase_t *sb);


LIBSBML_EXTERN
char*
SBase_getSBOTermID (const SBase_t *sb);


LIBSBML_EXTERN
unsigned int
SBase_getLine (const SBase_t *sb);


LIBSBML_EXTERN
unsigned int
SBase_getColumn (const SBase_t *sb);


LIBSBML_EXTERN
XMLNode_t *
SBase_getNotes (SBase_t *sb);


LIBSBML_EXTERN
char*
SBase_getNotesString (SBase_t *sb);


LIBSBML_EXTERN
XMLNode_t *
SBase_getAnnotation (SBase_t *sb);


LIBSBML_EXTERN
char*
SBase_getAnnotationString (SBase_t *sb);


LIBSBML_EXTERN
int
SBase_isSetMetaId (const SBase_t *sb);




LIBSBML_EXTERN
int
SBase_isSetNotes (const SBase_t *sb);


LIBSBML_EXTERN
int
SBase_isSetAnnotation (const SBase_t *sb);


LIBSBML_EXTERN
int
SBase_isSetSBOTerm(const SBase_t *sb);


LIBSBML_EXTERN
int
SBase_setMetaId (SBase_t *sb, const char *metaid);




LIBSBML_EXTERN
int
SBase_setSBOTerm (SBase_t *sb, int value);


LIBSBML_EXTERN
int
SBase_setSBOTermID (SBase_t *sb, const char* sboid);


LIBSBML_EXTERN
int
SBase_setNamespaces (SBase_t *sb, XMLNamespaces_t *xmlns);


LIBSBML_EXTERN
int
SBase_setNotes (SBase_t *sb, XMLNode_t *notes);


LIBSBML_EXTERN
int
SBase_setNotesString (SBase_t *sb, char *notes);


LIBSBML_EXTERN
int
SBase_appendNotes (SBase_t *sb, XMLNode_t *notes);


LIBSBML_EXTERN
int
SBase_appendNotesString (SBase_t *sb, char *notes);


LIBSBML_EXTERN
int
SBase_setAnnotation (SBase_t *sb, XMLNode_t *annotation);


LIBSBML_EXTERN
int
SBase_setAnnotationString (SBase_t *sb, char *annotation);


LIBSBML_EXTERN
int
SBase_appendAnnotation (SBase_t *sb, XMLNode_t *annotation);


LIBSBML_EXTERN
int
SBase_appendAnnotationString (SBase_t *sb, char *annotation);


LIBSBML_EXTERN
int
SBase_unsetMetaId (SBase_t *sb);




LIBSBML_EXTERN
int
SBase_unsetNotes (SBase_t *sb);


LIBSBML_EXTERN
int
SBase_unsetAnnotation (SBase_t *sb);


LIBSBML_EXTERN
int
SBase_unsetSBOTerm (SBase_t *sb);


LIBSBML_EXTERN
int 
SBase_addCVTerm(SBase_t *sb, CVTerm_t *term);


LIBSBML_EXTERN
int 
SBase_addCVTermNewBag(SBase_t *sb, CVTerm_t *term);


LIBSBML_EXTERN
List_t* 
SBase_getCVTerms(SBase_t *sb);


LIBSBML_EXTERN
unsigned int 
SBase_getNumCVTerms(SBase_t *sb);


LIBSBML_EXTERN
CVTerm_t* 
SBase_getCVTerm(SBase_t *sb, unsigned int n);


LIBSBML_EXTERN
int 
SBase_unsetCVTerms(SBase_t *sb);


LIBSBML_EXTERN
ModelHistory_t * 
SBase_getModelHistory(SBase_t *sb);

LIBSBML_EXTERN
int 
SBase_isSetModelHistory(SBase_t *sb);


LIBSBML_EXTERN
int 
SBase_setModelHistory(SBase_t *sb, ModelHistory_t *history);

LIBSBML_EXTERN
int 
SBase_unsetModelHistory(SBase_t *sb);


LIBSBML_EXTERN
BiolQualifierType_t 
SBase_getResourceBiologicalQualifier(SBase_t *sb, const char * resource);


LIBSBML_EXTERN
ModelQualifierType_t 
SBase_getResourceModelQualifier(SBase_t *sb, const char * resource);


LIBSBML_EXTERN
const Model_t *
SBase_getModel (const SBase_t *sb);


LIBSBML_EXTERN
unsigned int
SBase_getLevel (const SBase_t *sb);


LIBSBML_EXTERN
unsigned int
SBase_getVersion (const SBase_t *sb);


LIBSBML_EXTERN
SBMLTypeCode_t
SBase_getTypeCode (const SBase_t *sb);


LIBSBML_EXTERN
const char *
SBase_getElementName (const SBase_t *sb);


LIBSBML_EXTERN
int
SBase_hasValidLevelVersionNamespaceCombination(SBase_t *sb);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG   */
#endif  /* SBase_h */
