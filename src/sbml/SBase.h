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
 * @brief LibSBML implementation of %SBase, the base class of all SBML objects.
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
 * permitted inside the "notes" element; these are described separately
 * below.
 *
 * SBase has another optional subelement called "annotation".  Whereas the
 * "notes" element described above is a container for content to be shown
 * directly to humans, the "annotation" element is a container for optional
 * software-generated content @em not meant to be shown to humans.  The
 * element's content type is XML type @c any, allowing essentially
 * arbitrary data content.  SBML places only a few restrictions on the
 * organization of the content; these are intended to help software tools
 * read and write the data as well as help reduce conflicts between
 * annotations added by different tools.  They are described separately
 * below.
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
 * Beginning with SBML Level 2, SBase also has an optional attribute named
 * "metaid" for supporting metadata annotations using RDF (Resource
 * Description Format). The attribute value has the data type 
 * <a href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>, the
 * XML identifier type, which means each "metaid" value must be globally
 * unique within an SBML file.  (Importantly, this uniqueness criterion
 * applies across any attribute with type 
 * <a href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>, not just the "metaid"
 * attribute used by SBML&mdash;something to be aware of if your
 * application-specific XML content inside the "annotation" subelement
 * happens to use <a href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>.)
 * The "metaid" value serves to identify a model
 * component for purposes such as referencing that component from metadata
 * placed within "annotation" subelements.
 *
 * Beginning with SBML Level 2 Version 3, SBase also has an optional
 * attribute named "sboTerm" for supporting the use of the Systems Biology
 * Ontology.  In SBML proper, the data type of the attribute is a string of
 * the form SBO:NNNNNNN, where NNNNNNN is a seven digit integer number;
 * libSBML simplifies the representation by only storing the NNNNNNN
 * integer portion.  Thus, in libSBML, the "sboTerm" attribute on SBase has
 * data type @c int, and SBO identifiers are stored simply as integers.
 * SBO terms are a type of optional annotation, and each different class of
 * SBML object derived from SBase imposes its own requirements about the
 * values permitted for "sboTerm".  Please consult the SBML Level 2 Version
 * 4 specification for more information about the use of SBO and the
 * "sboTerm" attribute.
 *
 * Finally, note that, in the list of methods on SBase, there is no public
 * constructor because SBase is an abstract class.  The constructors reside
 * in the subclasses derived from SBase.
 *
 * 
 * @section sbase-notes Requirements for the content of the "notes" subelement
 *
 * The content of "notes" elements must be in XHTML 1.0 format.  (Plain
 * HTML would not be usable because whatever appears inside the "notes"
 * element must be compatible with XML, which HTML is not, and in any case,
 * the requirement for using XHTML does not prevent users from entering
 * plain-text content ,which they can do using the standard <code>&lt;pre&gt;
 * ... &lt;/pre&gt;</code> elements of [X]HTML.)
 *
 * The XML content of a "notes" subelement must declare the use of the
 * XHTML XML namespace.  This can be done in multiple ways.  One way is to
 * place a namespace declaration for the appropriate namespace URI (which
 * for XHTML is <tt>"http://www.w3.org/1999/xhtml"</tt>) on the top-level
 * <code>sbml</code> element and then reference the namespace in the
 * "notes" element content using a prefix.  The following example
 * illustrates this approach:
 * @verbatim
<sbml xmlns="http://www.sbml.org/sbml/level2/version4" level="2" version="4"
      xmlns:xhtml="http://www.w3.org/1999/xhtml">
  ...
  <notes>
    <xhtml:body>
      <xhtml:center><xhtml:h2>A Simple Mitotic Oscillator</xhtml:h2></xhtml:center>
      <xhtml:p>A minimal cascade model for the mitotic oscillator
      involving cyclin and cdc2 kinase</xhtml:p>
    </xhtml:body>
  </notes>
  ...
@endverbatim
 *
 * Another approach is to declare the XHTML namespace within the "notes"
 * element content itself, as in the following example:
 * @verbatim
...
<notes>
  <body xmlns="http://www.w3.org/1999/xhtml">

    <center><h2>A Simple Mitotic Oscillator</h2></center>

    <p>A minimal cascade model for the mitotic oscillator
    involving cyclin and cdc2 kinase</p>

  </body>
</notes>
...
@endverbatim
 *
 * The <code>xmlns="http://www.w3.org/1999/xhtml"</code> declaration on @c
 * body as shown above changes the default XML namespace within it, such
 * that all of its content is by default in the XHTML namespace.  This is a
 * particularly convenient approach because it obviates the need to prefix
 * every element with a namespace prefix (i.e., <code>xhtml:</code>
 * in the previous case).  Other approaches are also possible.
 * 
 * SBML does not require the content of the "notes" subelement to be any
 * particular XHTML element; the content can be almost any well-formed
 * XHTML content.  SBML Level 2 Versions 2, 3 and 4 added some small
 * restrictions and clarifications for the allowable content in order to
 * promote greater interoperability between software tools.  The first
 * restriction comes from the requirements of XML: the "notes"
 * element must not contain an XML declaration nor a DOCTYPE declaration.
 * That is, "notes" must @em not contain
 * @verbatim
<?xml version="1.0" encoding="UTF-8"?>  
@endverbatim
 * nor (where the following is only one specific example of a DOCTYPE
 * declaration)
 * @verbatim
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
      "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
@endverbatim
 *
 * The second restriction is intended to balance freedom of content with
 * the complexity of implementing software that can interpret the content.
 * The content of the "notes" subelement in SBML can consist only of the
 * following possibilities:
 * <ul>
 * <li> A complete XHTML document (minus the XML and DOCTYPE declarations,
 * of course), that is, XHTML content beginning with the @c html tag.
 * @verbatim
<notes>
  <html xmlns="http://www.w3.org/1999/xhtml">
     ...
  </html>
</notes>
@endverbatim
 *
 * <li> The @c body element from an XHTML document.  The following is an
 * example skeleton:
 * @verbatim
<notes>
    <body xmlns="http://www.w3.org/1999/xhtml">
      ...
    </body>
</notes>
@endverbatim
 *
 * <li> Any XHTML content that would be permitted within a @c body element.
 * If this consists of multiple elements, each one must declare the XML
 * namespace separately.  The following is an example fragment:
 * @verbatim
<notes>
    <p xmlns="http://www.w3.org/1999/xhtml">
      ...
    </p>
    <p xmlns="http://www.w3.org/1999/xhtml">
      ...
    </p>
</notes>
@endverbatim
 * </ul>
 *
 * Another way to summarize the restrictions above is simply to say that
 * the content of an SBML "notes" element can be only be a complete @c html
 * element, a @c body element, or whatever is permitted inside a @c body
 * element.  In practice, this does not limit in any meaningful way what
 * can be placed inside a "notes" element; for example, if an application
 * or modeler wants to put a complete XHTML page, including a @c head
 * element, it can be done by putting in everything starting with the @c
 * html container.  However, the restrictions above do make it somewhat
 * simpler to write software that can read and write the "notes" content.
 *
 * 
 * @section sbase-annotation Requirements for the content of the "annotation" subelement
 *
 * At the outset, software developers should keep in mind that multiple
 * software tools may attempt to read and write annotation content.  To
 * reduce the potential for collisions between annotations written by
 * different applications, SBML Level 2 stipulates that tools must use XML
 * namespaces to specify the intended vocabulary of every annotation.  The
 * application's developers must choose a URI (Universal Resource
 * Identifier) reference that uniquely identifies the vocabulary the
 * application will use, and a prefix string for the annotations.
 *
 * A important requirement is that application-specific annotation data is
 * entirely contained inside a single <em>top-level element</em> within the
 * SBML "annotation" subelement.  SBML Level 2 Versions 2, 3 and 4 place the
 * following restrictions on annotations:
 * <ul>
 * <li> Within a given SBML "annotation" element, there can only be one
 * top-level element using a given namespace.  An annotation element can
 * contain multiple top-level elements but each must be in a different
 * namespace.
 *
 * <li> No top-level element in an "annotation" may use an SBML XML
 * namespace, either explicitly by referencing one of the SBML XML
 * namespace URIs or implicitly by failing to specify any namespace on the
 * annotation.
 *
 * <li> The ordering of top-level elements within a given "annotation"
 * element is <em>not</em> significant.  An application should not expect
 * that its annotation content appears first in the "annotation" element,
 * nor in any other particular location.  Moreover, the ordering of
 * top-level annotation elements may be changed by different applications
 * as they read and write the same SBML file.
 * </ul>
 * 
 * The use of XML namespaces in this manner is intended to improve the
 * ability of multiple applications to place annotations on SBML model
 * elements with reduced risks of interference or name collisions.
 * Annotations stored by different simulation packages can therefore
 * coexist in the same model definition.  The rules governing the content
 * of "annotation" elements are designed to enable applications to easily
 * add, change, and remove their annotations from SBML elements while
 * simultaneously preserving annotations inserted by other applications
 * when mapping SBML from input to output.
 *
 * As a further simplification for developers of software and to improve
 * software interoperability, applications are only required to preserve
 * other annotations (i.e., annotations they do not recognize) when those
 * annotations are self-contained entirely within "annotation", complete
 * with namespace declarations.  The following is an example:
 * @verbatim
<annotation>
    <topLevelElement xmlns:"URI">
       ... content in the namespace identified by "URI" ...
    </topLevelElement>
</annotation>
@endverbatim
 *
 * Some more examples hopefully will make these points more clear.  The
 * next example is invalid because it contains a top-level element in the
 * SBML XML namespace&mdash;this happens because no namespace is declared
 * for the <code>&lt;cytoplasm&gt;</code> element, which means by default it
 * falls into the enclosing SBML namespace:
 * @verbatim
<annotation>
    <cytoplasm/>
</annotation>
@endverbatim
 *
 * The following example is also invalid, this time because it contains two
 * top-level elements using the same XML namespace.  Note that it does not
 * matter that these are two different top-level elements
 * (<code>&lt;nodecolors&gt;</code> and <code>&lt;textcolors&gt;</code>); 
 * what matters is that these separate elements are both in the same 
 * namespace rather than having been collected and placed inside one 
 * overall container element for that namespace.
 * @verbatim
<annotation>
    <mysim:nodecolors xmlns:mysim="http://www.mysim.org/ns"
        mysim:bgcolor="green" mysim:fgcolor="white"/>
    <mysim:textcolors xmlns:mysim="http://www.mysim.org/ns"
        mysim:bgcolor="green" mysim:fgcolor="white"/>
</annotation>
@endverbatim
 *
 * On the other hand, the following example is valid:
 * @verbatim
<annotation>
    <mysim:geometry xmlns:mysim="http://www.mysim.org/ns"
             mysim:bgcolor="green" mysim:fgcolor="white">
        <graph:node xmlns:graph="http://www.graph.org/ns" 
             graph:x="4" graph:y="5" />
    </mysim:geometry>
    <othersim:icon xmlns:othersim="http://www.othersim.com/">
        WS2002
    </othersim:icon>
</annotation>
@endverbatim
 *
 * It is worth keeping in mind that although XML namespace names must be
 * URIs, they are (like all XML namespace names) <em>not required</em> to
 * be directly usable in the sense of identifying an actual, retrieval
 * document or resource on the Internet.  URIs such as
 * <code>"http://www.mysim.org/"</code> may appear as though they are
 * (e.g.) Internet addresses, but there are not the same thing.  This
 * style of URI strings, using a domain name and other parts, is only a
 * simple and commonly-used way of creating a unique name string.
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
  SBMLConstructorException ();

};
/** @endcond */


class LIBSBML_EXTERN SBase
{
public:

  /**
   * Destroy this object.
   */
  virtual ~SBase ();


  /**
   * Assignment operator for SBase.
   */
  SBase& operator=(const SBase& orig);


  /**
   * Accepts the given SBMLVisitor for this instance of SBase.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>.
   */
  virtual bool accept (SBMLVisitor& v) const = 0;


  /**
   * Creates and returns a deep copy of this SBase object.
   * 
   * @return a (deep) copy of this SBase object
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
   * a tree of XML nodes.
   *
   * The optional element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments
   * about the model element in which the "notes" element is
   * enclosed.  Every object derived directly or indirectly from type
   * SBase can have a separate value for "notes", allowing users
   * considerable freedom when adding comments to their models.
   * The format of "notes" elements must be XHTML 1.0.  The SBML
   * Level&nbsp;2 specification has considerable detail about how
   * "notes" element content must be handled; please refer to the
   * specifications.
   *
   * The "notes" element content returned by this method will be in XML
   * form, but libSBML does not provide an object model specifically for
   * the content of notes.  Callers will need to traverse the XML tree
   * structure using the facilities available on XMLNode and related
   * objects.
   *
   * @return the content of the "notes" subelement of this SBML object.
   *
   * @see getNotesString()
   * @see isSetNotes()
   * @see setNotes(const XMLNode* notes)
   * @see setNotes(const std::string& notes)
   * @see appendNotes(const XMLNode* notes)
   * @see appendNotes(const std::string& notes)
   * @see unsetNotes()
   */
  XMLNode* getNotes();


  /**
   * Returns the content of the "notes" subelement of this object as a
   * string.
   *
   * The optional element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments
   * about the model element in which the "notes" element is
   * enclosed.  Every object derived directly or indirectly from type
   * SBase can have a separate value for "notes", allowing users
   * considerable freedom when adding comments to their models.
   * The format of "notes" elements must be XHTML 1.0.  The SBML
   * Level&nbsp;2 specification has considerable detail about how
   * "notes" element content must be handled; please refer to the
   * specifications.
   *
   * @return the content of the "notes" subelement of this SBML object.
   *
   * @see getNotes()
   * @see isSetNotes()
   * @see setNotes(const XMLNode* notes)
   * @see setNotes(const std::string& notes)
   * @see appendNotes(const XMLNode* notes)
   * @see appendNotes(const std::string& notes)
   * @see unsetNotes()
   */
  std::string getNotesString ();


  /**
   * Returns the content of the "annotation" subelement of this object as
   * an XML node tree.
   *
   * Whereas the SBase "notes" subelement is a container for content to be
   * shown directly to humans, the "annotation" element is a container for
   * optional software-generated content @em not meant to be shown to
   * humans.  Every object derived from SBase can have its own value for
   * "annotation".  The element's content type is XML type
   * <code>any</code>, allowing essentially arbitrary well-formed XML data
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
   * @return the annotation of this SBML object as an XMLNode
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
   * Whereas the SBase "notes" subelement is a container for content to be
   * shown directly to humans, the "annotation" element is a container for
   * optional software-generated content @em not meant to be shown to
   * humans.  Every object derived from SBase can have its own value for
   * "annotation".  The element's content type is XML type
   * <code>any</code>, allowing essentially arbitrary well-formed XML data
   * content.
   *
   * SBML places a few restrictions on the organization of the content of
   * annotations; these are intended to help software tools read and write
   * the data as well as help reduce conflicts between annotations added by
   * different tools.  Please see the SBML specifications for more details.
   *
   * The annotations returned by this method will be in string form.
   *
   * @return the annotation string of this SBML object.
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
   * @return the XML Namespaces associated with this SBML object
   */
  virtual XMLNamespaces* getNamespaces() const ;


  /**
   * Returns the parent SBMLDocument object.
   *
   * LibSBML uses the class SBMLDocument as a top-level container for
   * storing SBML content and data associated with it (such as warnings and
   * error messages).  An SBML model in libSBML is contained inside an
   * SBMLDocument object.  SBMLDocument corresponds roughly to the class
   * <i>Sbml</i> defined in the SBML Level&nbsp;2 specification, but it
   * does not have a direct correspondence in SBML Level&nbsp;1.  (But, it
   * is created by libSBML no matter whether the model is Level&nbsp;1 or
   * Level&nbsp;2.)
   *
   * This method allows the SBMLDocument for the current object to be
   * retrieved.
   * 
   * @return the parent SBMLDocument object of this SBML object.
   */
  const SBMLDocument* getSBMLDocument () const;


  /**
   * Returns the parent SBMLDocument object.
   *
   * LibSBML uses the class SBMLDocument as a top-level container for
   * storing SBML content and data associated with it (such as warnings and
   * error messages).  An SBML model in libSBML is contained inside an
   * SBMLDocument object.  SBMLDocument corresponds roughly to the class
   * <i>Sbml</i> defined in the SBML Level&nbsp;2 specification, but it
   * does not have a direct correspondence in SBML Level&nbsp;1.  (But, it
   * is created by libSBML no matter whether the model is Level&nbsp;1 or
   * Level&nbsp;2.)
   *
   * This method allows the SBMLDocument for the current object to be
   * retrieved.
   * 
   * @return the parent SBMLDocument object of this SBML object.
   */
  SBMLDocument* getSBMLDocument ();


  /**
   * Returns the parent SBML object.
   *
   * This method is convenient when holding an object nested inside
   * other objects in an SBML model.  It allows direct access to the
   * <code>&lt;model&gt;</code> element containing it.
   * 
   * @return the parent SBML object of this SBML object.
   */
  SBase* getParentSBMLObject();


  /**
   * Returns the ancestor SBML object that corresponds to the 
   * given SBMLTypeCode_t.
   *
   * This function allows any object to determine its exact
   * location/function within a model. For example a StoichiometryMath
   * object has ancestors of type SpeciesReference,
   * ListOfProducts/ListOfReactants/ListOfModifiers, Reaction,
   * ListOfReactions and Model; any of which can be accessed via this
   * function.
   *
   * @param type the SBMLTypeCode_t of the ancestor to be returned.
   * 
   * @return the ancestor SBML object of this SBML object, that 
   * corresponds to the given SBMLTypeCode_t, NULL if none exists.
   */
  SBase* getAncestorOfType(SBMLTypeCode_t type);


  /**
   * Returns the integer portion of the value of the "sboTerm" attribute of
   * this object.
   *
   * In SBML Level 2 Versions 2, 3 and 4, the data type of the attribute is a
   * string of the form SBO:NNNNNNN, where NNNNNNN is a seven digit integer
   * number; libSBML simplifies the representation by only storing the
   * NNNNNNN integer portion.  Thus, in libSBML, the "sboTerm" attribute on
   * SBase has data type @c int, and SBO identifiers are stored simply as
   * integers.  SBO terms are a type of optional annotation, and each
   * different class of SBML object derived from SBase imposes its own
   * requirements about the values permitted for "sboTerm".  Please consult
   * the SBML Level 2 Version 4 specification for more information about
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
   * In SBML Level 2 Versions 2, 3 and 4, the data type of the attribute is a
   * string of the form SBO:NNNNNNN, where NNNNNNN is a seven digit integer
   * number; libSBML simplifies the representation by only storing the
   * NNNNNNN integer portion.  Thus, in libSBML, the "sboTerm" attribute on
   * SBase has data type @c int, and SBO identifiers are stored simply as
   * integers.  This function recreates the string representation from the
   * stored value.  SBO terms are a type of optional annotation, and each
   * different class of SBML object derived from SBase imposes its own
   * requirements about the values permitted for "sboTerm".  Please consult
   * the SBML Level 2 Version 4 specification for more information about
   * the use of SBO and the "sboTerm" attribute.
   *
   * @return the value of the "sboTerm" attribute as a string of the form
   * SBO:NNNNNNN, or  an empty string
   * if the value is not set.
   */
  std::string getSBOTermID () const;


  /**
   * Returns the line number on which this object first appears in the XML
   * representation of the SBML document.
   * 
   * @return the line number of this SBML object.
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
   * @see getLine()
   */
  unsigned int getColumn () const;


  /**
   * Returns the ModelHistory of this SBase object.
   * 
   * @return ModelHistory of this SBase object.
   */
  ModelHistory* getModelHistory() const;


  /**
   * Returns the ModelHistory of this SBase object.
   * 
   * @return ModelHistory of this SBase object.
   */
  ModelHistory* getModelHistory();


  /**
   * Predicate returning @c true or @c false depending on whether this
   * object's "metaid" attribute has been set.
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
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return @c true if the "metaid" attribute of this SBML object has been
   * set, @c false otherwise.
   *
   * @see getMetaId()
   * @see setMetaId(const std::string& metaid)
   */
  bool isSetMetaId () const;


  /** @cond doxygen-libsbml-internal */
  /*
   * NOTE: THIS IS FOR BACKWARD COMPATABILITY REASONS
   * Predicate returning @c true or @c false depending on whether this
   * object's "id" attribute has been set.
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
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @return @c true if the "id" attribute of this SBML object has been
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
   * Predicate returning @c true or @c false depending on whether this
   * object's "name" attribute has been set.
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
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @return @c true if the "name" attribute of this SBML object has been
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
   * Predicate returning @c true or @c false depending on whether this
   * object's "notes" subelement exists and has content.
   *
   * The optional element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments
   * about the model element in which the "notes" element is
   * enclosed.  Every object derived directly or indirectly from type
   * SBase can have a separate value for "notes", allowing users
   * considerable freedom when adding comments to their models.
   * The format of "notes" elements must be XHTML 1.0.  The SBML
   * Level&nbsp;2 specification has considerable detail about how
   * "notes" element content must be handled; please refer to the
   * specifications.
   *
   * @htmlinclude libsbml-comment-set-methods.html
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
   */
  bool isSetNotes () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * object's "annotation" subelement exists and has content.
   *
   * Whereas the SBase "notes" subelement is a container for content to be
   * shown directly to humans, the "annotation" element is a container for
   * optional software-generated content @em not meant to be shown to
   * humans.  Every object derived from SBase can have its own value for
   * "annotation".  The element's content type is XML type
   * <code>any</code>, allowing essentially arbitrary well-formed XML data
   * content.
   *
   * SBML places a few restrictions on the organization of the content of
   * annotations; these are intended to help software tools read and write
   * the data as well as help reduce conflicts between annotations added by
   * different tools.  Please see the SBML specifications for more details.
   *
   * @htmlinclude libsbml-comment-set-methods.html
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
   * Predicate returning @c true or @c false depending on whether this
   * object's "sboTerm" attribute has been set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @return @c true if the "sboTerm" attribute of this SBML object has been
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
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param metaid the identifier string to use as the value of the
   * "metaid" attribute
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   * @li LIBSBML_UNEXPECTED_ATTRIBUTE
   * 
   * @see getMetaId()
   * @see isSetMetaId()
   */
  int setMetaId (const std::string& metaid);


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Model's ModelHistory has been set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @return @c true if the ModelHistory of this SBase object
   * has been set, @c false otherwise.
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
   * for the syntax of identifiers.  The following is summary of the
   * definition of the SBML identifier type @c SId (here expressed in an
   * extended form of BNF notation):
   * @code
   *   letter ::= 'a'..'z','A'..'Z'
   *   digit  ::= '0'..'9'
   *   idChar ::= letter | digit | '_'
   *   SId    ::= ( letter | '_' ) idChar*
   * @endcode
   * The equality of SBML identifiers is determined by an exact character
   * sequence match; i.e., comparisons must be performed in a
   * case-sensitive manner.  In addition, there are a few conditions for
   * the uniqueness of identifiers in an SBML model.  Please consult the
   * SBML specifications for the exact formulations.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param sid the string to use as the identifier of this object
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
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
   * @htmlinclude libsbml-comment-set-methods.html
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
   * @see getName()
   * @see isSetName()
   * @see unsetName()
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int setName (const std::string& name);
  /** @endcond */



  /**
   * Resets the value of the "annotation" subelement of this SBML object to a
   * copy of @p annotation.
   * 
   * Whereas the SBase "notes" subelement is a container for content to be
   * shown directly to humans, the "annotation" element is a container for
   * optional software-generated content @em not meant to be shown to
   * humans.  Every object derived from SBase can have its own value for
   * "annotation".  The element's content type is XML type
   * <code>any</code>, allowing essentially arbitrary well-formed XML data
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
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param annotation an XML structure that is to be used as the new content
   * of the "annotation" subelement of this object
   *
   * @see getAnnotationString()
   * @see isSetAnnotation()
   * @see setAnnotation(const std::string& annotation)
   * @see appendAnnotation(const XMLNode* annotation)
   * @see appendAnnotation(const std::string& annotation)
   * @see unsetAnnotation()
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
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
   * Sets the value of the "annotation" subelement of this SBML object to a
   * copy of @p annotation given as a character string.
   * 
   * Whereas the SBase "notes" subelement is a container for content to be
   * shown directly to humans, the "annotation" element is a container for
   * optional software-generated content @em not meant to be shown to
   * humans.  Every object derived from SBase can have its own value for
   * "annotation".  The element's content type is XML type
   * <code>any</code>, allowing essentially arbitrary well-formed XML data
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
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param annotation an XML string that is to be used as the content
   * of the "annotation" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
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
   * Appends the annotation content given by @p annotation to any existing
   * content in the "annotation" subelement of this object.
   * 
   * Whereas the SBase "notes" subelement is a container for content to be
   * shown directly to humans, the "annotation" element is a container for
   * optional software-generated content @em not meant to be shown to
   * humans.  Every object derived from SBase can have its own value for
   * "annotation".  The element's content type is XML type
   * <code>any</code>, allowing essentially arbitrary well-formed XML data
   * content.
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
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @param annotation an XML structure that is to be copied and appended
   * to the content of the "annotation" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
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
   * Appends the annotation content given by @p annotation to any existing
   * content in the "annotation" subelement of this object.
   *
   * Whereas the SBase "notes" subelement is a container for content to be
   * shown directly to humans, the "annotation" element is a container for
   * optional software-generated content @em not meant to be shown to
   * humans.  Every object derived from SBase can have its own value for
   * "annotation".  The element's content type is XML type
   * <code>any</code>, allowing essentially arbitrary well-formed XML data
   * content.
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
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @param annotation an XML string that is to be copied and appended
   * to the content of the "annotation" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
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
   * Sets the value of the "notes" subelement of this SBML object to a copy
   * of the XML structure given by @p notes.
   *
   * Any existing content of the "notes" subelement is discarded.
   *
   * The optional element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments
   * about the model element in which the "notes" element is
   * enclosed.  Every object derived directly or indirectly from type
   * SBase can have a separate value for "notes", allowing users
   * considerable freedom when adding comments to their models.
   * The format of "notes" elements must be XHTML 1.0.  The SBML
   * Level&nbsp;2 specification has considerable detail about how
   * "notes" element content must be handled; please refer to the
   * specifications.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param notes an XML structure that is to be used as the content of the
   * "notes" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_OBJECT
   *
   * @see getNotesString()
   * @see isSetNotes()
   * @see setNotes(const std::string& notes)
   * @see appendNotes(const XMLNode* notes)
   * @see appendNotes(const std::string& notes)
   * @see unsetNotes()
   */
  int setNotes(const XMLNode* notes);


  /**
   * Sets the value of the "notes" subelement of this SBML object to a copy
   * of the string @p notes.
   *
   * Any existing content of the "notes" subelement is discarded.
   *
   * The optional element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments
   * about the model element in which the "notes" element is
   * enclosed.  Every object derived directly or indirectly from type
   * SBase can have a separate value for "notes", allowing users
   * considerable freedom when adding comments to their models.
   * The format of "notes" elements must be XHTML 1.0.  The SBML
   * Level&nbsp;2 specification has considerable detail about how
   * "notes" element content must be handled; please refer to the
   * specifications.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param notes an XML string that is to be used as the content of the
   * "notes" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_OBJECT
   * @li LIBSBML_OPERATION_FAILED
   *
   * @see getNotesString()
   * @see isSetNotes()
   * @see setNotes(const XMLNode* notes)
   * @see appendNotes(const XMLNode* notes)
   * @see appendNotes(const std::string& notes)
   * @see unsetNotes()
   */
  int setNotes(const std::string& notes);


  /**
   * Appends notes content to the "notes" element attached to this
   * object.
   *
   * The content in @p notes is copied.
   *
   * The optional element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments
   * about the model element in which the "notes" element is
   * enclosed.  Every object derived directly or indirectly from type
   * SBase can have a separate value for "notes", allowing users
   * considerable freedom when adding comments to their models.
   * The format of "notes" elements must be XHTML 1.0.  The SBML
   * Level&nbsp;2 specification has considerable detail about how
   * "notes" element content must be handled; please refer to the
   * specifications.
   * 
   * @param notes an XML node structure that is to appended to the content
   * of the "notes" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_OBJECT
   * @li LIBSBML_OPERATION_FAILED
   *
   * @see getNotesString()
   * @see isSetNotes()
   * @see setNotes(const XMLNode* notes)
   * @see setNotes(const std::string& notes)
   * @see appendNotes(const std::string& notes)
   * @see unsetNotes()
   */
  int appendNotes(const XMLNode* notes);


  /**
   * Appends notes content to the "notes" element attached to this
   * object.
   *
   * The content in @p notes is copied.
   *
   * The optional element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments
   * about the model element in which the "notes" element is
   * enclosed.  Every object derived directly or indirectly from type
   * SBase can have a separate value for "notes", allowing users
   * considerable freedom when adding comments to their models.
   * The format of "notes" elements must be XHTML 1.0.  The SBML
   * Level&nbsp;2 specification has considerable detail about how
   * "notes" element content must be handled; please refer to the
   * specifications.
   *
   * @param notes an XML string that is to appended to the content of
   * the "notes" subelement of this object
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_OBJECT
   * @li LIBSBML_OPERATION_FAILED
   *
   * @see getNotesString()
   * @see isSetNotes()
   * @see setNotes(const XMLNode* notes)
   * @see setNotes(const std::string& notes)
   * @see appendNotes(const XMLNode* notes)
   * @see unsetNotes()
   */
  int appendNotes(const std::string& notes);


  /**
   * Sets the ModelHistory of this SBase object.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @param history ModelHistory of this SBase object.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_OBJECT
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
   * In SBML Level 2 Versions 2, 3 and 4, the data type of the SBML "sboTerm"
   * attribute is a string of the form SBO:NNNNNNN, where NNNNNNN is a
   * seven digit integer number; libSBML simplifies the representation by
   * only storing the NNNNNNN integer portion.  Thus, in libSBML, the
   * "sboTerm" attribute on SBase has data type @c int, and SBO identifiers
   * are stored simply as integers.  SBO terms are a type of optional
   * annotation, and each different class of SBML object derived from SBase
   * imposes its own requirements about the values permitted for "sboTerm".
   * Please consult the SBML Level 2 Version 4 specification for more
   * information about the use of SBO and the "sboTerm" attribute.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param value the NNNNNNN integer portion of the SBO identifier
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   * @li LIBSBML_UNEXPECTED_ATTRIBUTE
   */
  virtual int setSBOTerm (int value);

  /*
   * Sets the value of the "sboTerm" attribute by string.
   *
   * In SBML Level 2 Versions 2, 3 and 4, the data type of the SBML "sboTerm"
   * attribute is a string of the form SBO:NNNNNNN, where NNNNNNN is a
   * seven digit integer number; libSBML simplifies the representation by
   * only storing the NNNNNNN integer portion converted from the given string.  
   * Thus, in libSBML, the "sboTerm" attribute on SBase has data type @c int, 
   * and SBO identifiers are stored simply as integers.  SBO terms are a type
   * of optional annotation, and each different class of SBML object derived 
   * from SBase imposes its own requirements about the values permitted for 
   * "sboTerm".  Please consult the SBML Level 2 Version 4 specification for 
   * more information about the use of SBO and the "sboTerm" attribute.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param value the SBO identifier string of the form SBO:NNNNNNN
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   * @li LIBSBML_UNEXPECTED_ATTRIBUTE
   */
  virtual int setSBOTerm (const std::string &sboid);

  /**
   * Sets the namespaces relevant of this SBML object.
   *
   * @param xmlns the namespaces to set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
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
   * The "metaid" value serves to identify a model
   * component for purposes such as referencing that component from
   * metadata placed within "annotation" subelements.
   *  
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
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
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   *
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
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
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   *
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  int unsetName ();
#endif


  /**
   * Unsets the value of the "notes" subelement of this SBML object.
   *
   * The optional element named "notes", present on every major SBML
   * component type, is intended as a place for storing optional
   * information intended to be seen by humans.  An example use of the
   * "notes" element would be to contain formatted user comments
   * about the model element in which the "notes" element is
   * enclosed.  Every object derived directly or indirectly from type
   * SBase can have a separate value for "notes", allowing users
   * considerable freedom when adding comments to their models.
   * The format of "notes" elements must be XHTML 1.0.  The SBML
   * Level&nbsp;2 specification has considerable detail about how
   * "notes" element content must be handled; please refer to the
   * specifications.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * 
   * @see getNotesString()
   * @see isSetNotes()
   * @see setNotes(const XMLNode* notes)
   * @see setNotes(const std::string& notes)
   * @see appendNotes(const XMLNode* notes)
   * @see appendNotes(const std::string& notes)
   */
  int unsetNotes ();


  /**
   * Unsets the value of the "annotation" subelement of this SBML object.
   *
   * Whereas the SBase "notes" subelement is a container for content to be
   * shown directly to humans, the "annotation" element is a container for
   * optional software-generated content @em not meant to be shown to
   * humans.  Every object derived from SBase can have its own value for
   * "annotation".  The element's content type is XML type
   * <code>any</code>, allowing essentially arbitrary well-formed XML data
   * content.
   *
   * SBML places a few restrictions on the organization of the content of
   * annotations; these are intended to help software tools read and write
   * the data as well as help reduce conflicts between annotations added by
   * different tools.  Please see the SBML specifications for more details.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
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
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_UNEXPECTED_ATTRIBUTE
   */
  int unsetSBOTerm ();


  /**
   * Adds a copy of the given CVTerm to this SBML object.
   *
   * @param term the CVTerm to assign
   * @param newBag boolean indicating whether to create a new bag
   * with the same identifier as a previous bag
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   * @li LIBSBML_UNEXPECTED_ATTRIBUTE
   * @li LIBSBML_INVALID_OBJECT
   *
   * @note Since the CV Term uses the metaid of the object as a 
   * reference, if the object has no metaid set the CVTerm will
   * not be added.
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
   * Clears the list of CVTerms of this SBML
   * object.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  int unsetCVTerms();


  /**
   * Unsets the ModelHistory of this SBase object.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  int unsetModelHistory();


  /**
   * Returns the BiologicalQualifier associated with this resource,
   * or BQB_UNKNOWN if the resource does not exist.
   *
   * @param resource string representing the resource; e.g.,
   * "http://www.geneontology.org/#GO:0005892"
   *
   * @return the #BiolQualifierType_t value associated with the resource
   */
  BiolQualifierType_t getResourceBiologicalQualifier(std::string resource);


  /**
   * Returns the ModelQualifier associated with this resource,
   * or BQM_UNKNOWN if the resource does not exist.
   *
   * @param resource string representing the resource; e.g.,
   * "http://www.geneontology.org/#GO:0005892"
   *
   * @return the #ModelQualifierType_t value associated with the resource
   */
  ModelQualifierType_t getResourceModelQualifier(std::string resource);


  /**
   * Returns the Model object in which the current object is located.
   * 
   * @return the parent Model of this SBML object.
   */
  const Model* getModel () const;


  /**
   * Returns the SBML Level of the overall SBML document.
   * 
   * @return the SBML level of this SBML object.
   * 
   * @see getVersion()
   */
  unsigned int getLevel () const;


  /**
   * Returns the Version within the SBML Level of the overall SBML document.
   * 
   * @return the SBML version of this SBML object.
   *
   * @see getLevel()
   */
  unsigned int getVersion () const;


  /**
   * Returns the libSBML type code for this object.
   * 
   * This method MAY return the typecode of this SBML object or it MAY
   * return SBML_UNKNOWN.  That is, subclasses of SBase are not required to
   * implement this method to return a typecode.  This method is meant
   * primarily for the LibSBML C interface where class and subclass
   * information is not readily available.
   *
   * @return the #SBMLTypeCode_t value of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * object's level/version and namespace values correspond to a valid
   * SBML specification.
   *
   * The valid combinations of SBML Level, Version and Namespace as of this release
   * of libSBML are the following: 
   * <ul>
   * <li> Level&nbsp;1 Version&nbsp;2 "http://www.sbml.org/sbml/level1"
   * <li> Level&nbsp;2 Version&nbsp;1 "http://www.sbml.org/sbml/level2"
   * <li> Level&nbsp;2 Version&nbsp;2 "http://www.sbml.org/sbml/level2/version2"
   * <li> Level&nbsp;2 Version&nbsp;3 "http://www.sbml.org/sbml/level2/version3"
   * <li> Level&nbsp;2 Version&nbsp;4 "http://www.sbml.org/sbml/level2/version4"
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
   * SBML component.  For example, Model defines it as returning "model",
   * CompartmentType defines it as returning "compartmentType", etc.
   */
  virtual const std::string& getElementName () const = 0;


  /**
   * Returns a string that consists of the partial SBML describing this
   * object.  This is primarily provided for testing and debugging
   * purposes.  It may be removed in a future version of libSBML.
   * 
   * @return the partial SBML that describes this SBML object.
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
   * implementation of this method as well.  For example:
   *
   *   SBase::writeElements(stream);
   *   mReactans.write(stream);
   *   mProducts.write(stream);
   *   ...
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
  /** @endcond */


protected:
  /** @cond doxygen-libsbml-internal */

  /**
   * Only subclasses may create SBase objects.
   *
   * @docnote @htmlinclude libsbml-warn-default-args-in-docs.html
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
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);


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
   * siblings or -1 (default) to indicate the position is not significant.
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
   * @docnote @htmlinclude libsbml-warn-default-args-in-docs.html
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
   * Checks that SBML element has been read in the proper order.  If object
   * is not in the expected position, an error is logged.
   */
  void checkOrderAndLogError (SBase* object, int expected);


  /**
   * Checks that an SBML ListOf element has been populated.  
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
   * @docnote @htmlinclude libsbml-warn-default-args-in-docs.html
   */
  void checkUnitSyntax(unsigned int flag = 0);
#endif

  /**
   * Checks that the given default namespace in the given element is valid.
   * If the given default namespace is not valid, an error is logged.
   */
  void checkDefaultNamespace(const XMLNamespaces* xmlns, const std::string& elementName);

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
