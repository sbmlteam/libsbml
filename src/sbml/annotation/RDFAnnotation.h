/**
 * @file    RDFAnnotation.h
 * @brief   RDFAnnotation I/O
 * @author  Sarah Keating
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
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class RDFAnnotationParser
 * @brief Read/write/manipulate RDF annotations stored in SBML
 * annotation elements.
 *
 * @htmlinclude not-sbml-warning.html
 *
 * RDFAnnotationParser is a libSBML construct used as part of the libSBML
 * support for annotations conforming to the guidelines specified by MIRIAM
 * ("Minimum Information Requested in the Annotation of biochemical
 * Models", <i>Nature Biotechnology</i>, vol. 23, no. 12, Dec. 2005).
 * Section 6 of the SBML Level&nbsp;2 and Level&nbsp;3 specification
 * documents defines a recommended way of encoding MIRIAM information using
 * a subset of RDF in SBML.  The general scheme is as follows.  A set of
 * RDF-based annotations attached to a given SBML
 * <code>&lt;annotation&gt;</code> element are read by RDFAnnotationParser
 * and converted into a list of CVTerm objects.  There
 * are different versions of the main method,
 * @if clike RDFAnnotationParser::parseRDFAnnotation(const XMLNode *annotation, %List *CVTerms)
 * @endif@if java RDFAnnotationParser::parseRDFAnnotation(const XMLNode *annotation, %CVTermList *CVTerms) @endif
 * and RDFAnnotationParser::parseRDFAnnotation(const XMLNode *annotation), 
 * used depending on whether the annotation in question concerns the MIRIAM
 * model history or other MIRIAM resource annotations.  A special object
 * class, ModelHistory, is used to make it easier to manipulate model
 * history annotations.
 *
 * All of the methods on RDFAnnotationParser are static; the class exists
 * only to encapsulate the annotation and CVTerm parsing and manipulation
 * functionality.
 */

#ifndef RDFAnnotation_h
#define RDFAnnotation_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>

#include <sbml/xml/XMLAttributes.h>

#ifndef LIBSBML_USE_STRICT_INCLUDES
#include <sbml/annotation/ModelHistory.h>
#endif

#ifdef __cplusplus

#include <limits>
#include <iomanip>
#include <string>
#include <sstream>

#include <cstdlib>

LIBSBML_CPP_NAMESPACE_BEGIN

#ifdef LIBSBML_USE_STRICT_INCLUDES
class ModelHistory;
class ModelCreator;
class Date;
#endif

class XMLErrorLog;

class LIBSBML_EXTERN RDFAnnotationParser
{
public:

  /**
   * Parses an annotation (given as an XMLNode tree) into a list of
   * CVTerm objects.
   *
   * This is used to take an annotation that has been read into an SBML
   * model, identify the RDF elements within it, and create a list of
   * corresponding CVTerm (controlled vocabulary term) objects.
   *
   * @param annotation XMLNode containing the annotation.
   * 
   * @param CVTerms list of CVTerm objects to be created.
   * @param log optional error log to add warnings to.
   * @param metaId optional metaId, if set only the rdf annotation for this metaId will be returned.
   *
   * @see @if clike parseRDFAnnotation(const XMLNode *annotation) @else RDFAnnotationParser::parseRDFAnnotation(const XMLNode *annotation) @endif
   */
  static void parseRDFAnnotation(const XMLNode *annotation, List *CVTerms, XMLErrorLog* log = NULL, const char* metaId = NULL);


  /**
   * Parses an annotation into a ModelHistory class instance.
   *
   * This is used to take an annotation that has been read into an SBML
   * model, identify the RDF elements representing model history
   * information, and create a list of corresponding CVTerm objects.
   *
   * @param annotation XMLNode containing the annotation.
   * @param log optional error log to add warnings to.
   * @param metaId optional metaId, if set only the rdf annotation for this metaId will be returned.
   *
   * @return a pointer to the ModelHistory created.
   */
  static ModelHistory* parseRDFAnnotation(const XMLNode *annotation, XMLErrorLog* log = NULL, const char* metaId = NULL);


  /**
   * Creates a blank annotation and returns its root XMLNode object.
   *
   * This creates a completely empty SBML <code>&lt;annotation&gt;</code>
   * element.  It is not attached to any SBML element.  An example of how
   * this might be used is illustrated in the following code fragment.  In
   * this example, suppose that @c content is an XMLNode object previously
   * created, containing MIRIAM-style annotations, and that @c sbmlObject
   * is an SBML object derived from SBase (e.g., a Model, or a Species, or
   * a Compartment, etc.).  Then:@if clike
@verbatim
int success;                              // Status code variable, used below.

XMLNode *RDF = createRDFAnnotation();     // Create RDF annotation XML structure.
success = RDF->addChild(...content...);   // Put some content into it.
...                                       // Check "success" return code value.

XMLNode *ann = createAnnotation();        // Create <annotation> container.
success = ann->addChild(RDF);             // Put the RDF annotation into it.
...                                       // Check "success" return code value.

success = sbmlObject->setAnnotation(ann); // Set object's annotation to what we built.
...                                       // Check "success" return code value.
@endverbatim
   * @endif@if java
@verbatim
int success;                                   // Status code variable, used below.

XMLNode RDF = createRDFAnnotation();          // Create RDF annotation XML structure.
success      = RDF.addChild(...content...);    // Put some content into it.
...                                            // Check "success" return code value.

XMLNode ann = createAnnotation();             // Create <annotation> container.
success      = ann.addChild(RDF);              // Put the RDF annotation into it.
...                                            // Check "success" return code value.

success      = sbmlObject.setAnnotation(ann); // Set object's annotation to what we built.
...                                            // Check "success" return code value.
@endverbatim
   * @endif@if python
@verbatim
RDF     = RDFAnnotationParser.createRDFAnnotation() # Create RDF annotation XML structure.
success = RDF.addChild(...content...)               # Put some content into it.
...                                                 # Check "success" return code value.

annot   = RDFAnnotationParser.createAnnotation()    # Create <annotation> container.
success = annot.addChild(RDF)                       # Put the RDF annotation into it.
...                                                 # Check "success" return code value.

success = sbmlObject.setAnnotation(annot)           # Set object's annotation to what we built.
...                                                 # Check "success" return code value.
@endverbatim
   * @endif
   * The SBML specification contains more information about the format of
   * annotations.  We urge readers to consult Section&nbsp;6 of the SBML
   * Level&nbsp;2 (Versions 2&ndash;4) and SBML Level&nbsp;3 specification
   * documents.
   *
   * @return a pointer to an XMLNode for the annotation
   *
   * @see @if clike createRDFAnnotation() @else RDFAnnotationParser::createRDFAnnotation() @endif
   */
  static XMLNode * createAnnotation();

 
  /**
   * Creates a blank RDF element suitable for use in SBML annotations.
   *
   * The annotation created by this method has namespace declarations for
   * all the relevant XML namespaces used in RDF annotations and also has
   * an empty RDF element.  The result is the following XML:
@verbatim
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:dc="http://purl.org/dc/elements/1.1/"
         xmlns:dcterms="http://purl.org/dc/terms/"
         xmlns:vCard="http://www.w3.org/2001/vcard-rdf/3.0#"
         xmlns:bqbiol="http://biomodels.net/biology-qualifiers/"
         xmlns:bqmodel="http://biomodels.net/model-qualifiers/" >

</rdf:RDF>
@endverbatim
   *
   * Note that this does not create the containing SBML
   * <code>&lt;annotation&gt;</code> element; the method
   * @if clike createAnnotation()@else RDFAnnotationParser::createAnnotation()@endif
   * is available for creating the container.
   *
   * @return a pointer to an XMLNode
   *
   * @see @if clike createAnnotation() @else RDFAnnotationParser::createAnnotation() @endif
   */
  static XMLNode * createRDFAnnotation();


  /**
   * Deletes any RDF annotation found in the given XMLNode tree and returns
   * any remaining annotation content.
   *
   * The name of the XMLNode given as parameter @p annotation must be
   * "annotation", or else this method returns @c NULL.  The method will
   * walk down the XML structure looking for elements that are in the
   * RDF XML namespace, and remove them.
   *
   * @param annotation the XMLNode tree within which the RDF annotation is
   * to be found and deleted
   *
   * @return the XMLNode structure that is left after RDF annotations are
   * deleted.
   */
  static XMLNode * deleteRDFAnnotation(const XMLNode *annotation);


  /**
   * Takes an SBML object and creates an empty XMLNode corresponding to an
   * RDF "Description" element.
   *
   * This method is a handy way of creating RDF description objects linked
   * by the appropriate "metaid" field to the given @p object, for
   * insertion into RDF annotations in a model.  The method retrieves the
   * "metaid" attribute from the @p object passed in as argument, then
   * creates an empty element having the following form
   * (where <span class="code" style="background-color: #eed0d0">metaid</span> 
   * the value of the "metaid" attribute of the argument):
   * 
 <div class="fragment">
 &lt;rdf:Description rdf:about=&quot;#<span style="background-color: #eed0d0">metaid</span>&quot; xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"&gt;<br>
 ...<br>
 &lt;/rdf:Description&gt;<br>
 </div>
   * Note that this method does @em not create a complete annotation or
   * even an RDF element; it only creates the "Description" portion.  Callers
   * will need to use other methods such as
   * @if clike createRDFAnnotation()@else RDFAnnotationParser::createRDFAnnotation()@endif
   * to create the rest of the structure for an annotation.
   *
   * @param obj the object to which the "Description" refers
   *
   * @return a new XMLNode containing the "rdf:Description" element with
   * its "about" attribute value set to the @p object meta identifier.
   *
   * @see @if clike createRDFAnnotation() @else RDFAnnotationParser::createRDFAnnotation() @endif
   */
  static XMLNode * createRDFDescription(const SBase *obj);


  /**
   * Takes a list of CVTerm objects and creates a the RDF "Description"
   * element.
   *
   * This essentially takes the given SBML object, reads out the CVTerm objects
   * attached to it, calls
   * @if clike createRDFDescription()@else RDFAnnotationParser::createRDFDescription(SBase object)@endif
   * to create an RDF "Description" element to hold the terms and adds
   * each term with appropriate qualifiers.
   *
   * @param obj the SBML object to start from
   *
   * @return the XMLNode tree corresponding to the Description element of
   * an RDF annotation.
   */
  static XMLNode * createCVTerms(const SBase *obj);


  /**
   * Takes a list of CVTerm objects and creates a complete SBML annotation
   * around it.
   *
   * This essentially takes the given SBML object, calls
   * @if clike RDFAnnotationParser::createCVTerms()@else RDFAnnotationParser::createCVTerms(SBase obj)@endif
   * to read out the CVTerm objects
   * attached to it, calls
   * @if clike createRDFAnnotation()@else RDFAnnotationParser::createRDFAnnotation()@endif
   * to create an RDF
   * annotation to hold the terms, and finally calls
   * @if clike createAnnotation()@else RDFAnnotationParser::createAnnotation()@endif
   * to wrap the result as an SBML <code>&lt;annotation&gt;</code> element.
   *
   * @param obj the SBML object to start from
   *
   * @return the XMLNode tree corresponding to the annotation.
   */
  static XMLNode * parseCVTerms(const SBase * obj);


  /**
   * Reads the model history stored in @p obj and creates the
   * XML structure for an SBML annotation representing that history.
   *
   * @param obj any SBase object
   *
   * @return the XMLNode corresponding to an annotation containing 
   * MIRIAM-compliant model history information in RDF format.
   */
  static XMLNode * parseModelHistory(const SBase * obj);


  /** @cond doxygen-libsbml-internal */

  
  static bool hasRDFAnnotation(const XMLNode *annotation);


  static bool hasAdditionalRDFAnnotation(const XMLNode *annotation);


  static bool hasCVTermRDFAnnotation(const XMLNode *annotation);


  static bool hasHistoryRDFAnnotation(const XMLNode *annotation);

   /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/* ----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 * --------------------------------------------------------------------------*/

void
RDFAnnotationParser_parseRDFAnnotation(const XMLNode_t * annotation, 
                                       List_t *CVTerms);

ModelHistory_t *
RDFAnnotationParser_parseRDFAnnotationWithModelHistory(
                                        const XMLNode_t * annotation);

XMLNode_t *
RDFAnnotationParser_createAnnotation();

XMLNode_t *
RDFAnnotationParser_createRDFAnnotation();

XMLNode_t *
RDFAnnotationParser_deleteRDFAnnotation(XMLNode_t *annotation);

XMLNode_t *
RDFAnnotationParser_createRDFDescription(const SBase_t * obj);

XMLNode_t *
RDFAnnotationParser_createCVTerms(const SBase_t * obj);

XMLNode_t *
RDFAnnotationParser_parseCVTerms(const SBase_t * obj);

XMLNode_t *
RDFAnnotationParser_parseModelHistory(const SBase_t * obj);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /** RDFAnnotation_h **/

