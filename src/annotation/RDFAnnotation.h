/**
 * @file    RDFAnnotation.h
 * @brief   RDFAnnotation I/O
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
 * @class RDFAnnotationParser
 * @brief Read/write/manipulate RDF annotations stored in SBML
 * annotation elements.
 *
 * @htmlinclude libsbml-not-sbml-warning.html
 *
 * RDFAnnotationParser is a libSBML construct used as part of the libSBML
 * support for annotations conforming to the guidelines specified by MIRIAM
 * ("Minimum Information Requested in the Annotation of biochemical
 * Models", <i>Nature Biotechnology</i>, vol. 23, no. 12, Dec. 2005).  Section 6
 * of the SBML Level&nbsp;2 Version&nbsp;4 specification defines a recommended way
 * of encoding MIRIAM information as RDF annotations in SBML.  The general
 * scheme is as follows.  A set of RDF-based annotations attached to a
 * given SBML <code>&lt;annotation&gt;</code> element are read by
 * RDFAnnotationParser and converted into a list of CVTerm objects.  There
 * are different versions of the main method, @if clike RDFAnnotationParser::parseRDFAnnotation(const XMLNode *annotation, %List *CVTerms) @endif
 * @if java RDFAnnotationParser::parseRDFAnnotation(const XMLNode *annotation, CVTermList *CVTerms) @endif
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

#include <sbml/annotation/ModelHistory.h>


#ifdef __cplusplus

#include <limits>
#include <iomanip>
#include <string>
#include <sstream>

#include <cstdlib>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN RDFAnnotationParser
{
public:

  /**
   * Parses an annotation (given as an XMLNode tree) into a list of
   * CVTerms.
   *
   * This is used to take an annotation that has been read into an SBML
   * model, identify the RDF elements within it, and create a list of
   * corresponding CVTerms.
   *
   * @param annotation XMLNode containing the annotation.
   * 
   * @param CVTerms list of CVTerms to be created.
   *
   * @see parseRDFAnnotation(const XMLNode *annotation)
   */
  static void parseRDFAnnotation(const XMLNode *annotation, List *CVTerms);


  /**
   * Parses an annotation into a ModelHistory class instance.
   *
   * This is used to take an annotation that has been read into an SBML
   * model, identify the RDF elements representing model history
   * information, and create a list of corresponding CVTerms.
   *
   * @param annotation XMLNode containing the annotation.
   *
   * @return a pointer to the ModelHistory created.
   */
  static ModelHistory* parseRDFAnnotation(const XMLNode *annotation);


  /**
   * Creates a blank annotation and returns the XMLNode corresonding to it.
   *
   * The annotation created by this method is a completely empty SBML
   * <code>&lt;annotation&gt;</code> element.  One use for this is to
   * then call createRDFAnnotation() to construct RDF content for this
   * empty annotation.
   *
   * @return a pointer to an XMLNode for the annotation
   *
   * @see createRDFAnnotation()
   */
  static XMLNode * createAnnotation();

 
  /**
   * Creates blank RDF annotation content organized in the form defined in
   * Section 6 of the SBML Level 2 Version 4 specification .
   *
   * The annotation created by this method has namespace declarations for
   * all the relevant XML namespaces used in RDF annotations and also has
   * an empty RDF element.  Note that this is not the containing
   * <code>&lt;annotation&gt;</code> element; the method createAnnotation()
   * is available for that purpose.
   *
   * @return a pointer to an XMLNode
   */
  static XMLNode * createRDFAnnotation();


  /**
   * Deletes any RDF annotation found in the given XMLNode tree and returns
   * any remaining annotation content.
   *
   * The name of the given XMLNode must be "annotation", or else this
   * method returns NULL.
   *
   * @param annotation the annotation tree within which the RDF annotation
   * is to be found and deleted
   *
   * @return the XMLNode structure with any RDF annotations deleted
   */
  static XMLNode * deleteRDFAnnotation(const XMLNode *annotation);


  /**
   * Takes an SBML object and creates an XMLNode corresponding to an
   * RDF "Description" element.
   *
   * This method is a handy way of creating RDF description objects linked
   * by the appropriate "metaid" field, for insertion into RDF annotations
   * in a model.  (Note that this method does not create a complete
   * annotation; it only creates a description element.  For creating empty
   * RDF annotations that can serve as containers for RDF descriptions, see
   * createRDFAnnotation().
   *
   * @param object the object to be annotated
   *
   * @return a new XMLNode containing the "rdf:about" structure for an
   * RDF "Description" element.
   *
   * @see createRDFAnnotation()
   */
  static XMLNode * createRDFDescription(const SBase *object);


  /**
   * Takes a list of CVTerms and creates a the RDF "Description" element.
   *
   * This essentially takes the given SBML object, reads out the CVTerms
   * attached to it, calls createRDFDescriptiom() to create an RDF
   * "Description" element to hold the terms and adds each term with
   * appropriate qualifiers.
   *
   * @param object the SBML object to start from
   *
   * @return the XMLNode tree corresponding to the Description element of
   * an RDF annotation.
   */
  static XMLNode * createCVTerms(const SBase *object);


  /**
   * Takes a list of CVTerms and creates a complete SBML annotation
   * around it.
   *
   * This essentially takes the given SBML object, calls createCVTerms
   * to read out the CVTerms
   * attached to it, calls createRDFAnnotation() to create an RDF
   * annotation to hold the terms, and finally calls createAnnotation() to
   * wrap the result as an SBML <code>&lt;annotation&gt;</code> element.
   *
   * @param object the SBML object to start from
   *
   * @return the XMLNode tree corresponding to the annotation.
   */
  static XMLNode * parseCVTerms(const SBase * object);


  /**
   * Takes an SBML object, reads off the model history information
   * stored in it, and creates a complete SBML annotation to store that
   * history.
   *
   * @param object any SBase object
   *
   * @return the XMLNode corresponding to an annotation containing 
   * MIRIAM-compliant model history information in RDF format
   */
  static XMLNode * parseModelHistory(const SBase * object);


  /** @cond doxygen-libsbml-internal */

  
  static bool hasRDFAnnotation(const XMLNode *annotation);


  static bool hasAdditionalRDFAnnotation(const XMLNode *annotation);


  static bool hasCVTermRDFAnnotation(const XMLNode *annotation);


  static bool hasHistoryRDFAnnotation(const XMLNode *annotation);

   /** @endcond doxygen-libsbml-internal */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/

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
RDFAnnotationParser_createRDFDescription(const SBase_t * object);

XMLNode_t *
RDFAnnotationParser_createCVTerms(const SBase_t * object);

XMLNode_t *
RDFAnnotationParser_parseCVTerms(const SBase_t * object);

XMLNode_t *
RDFAnnotationParser_parseModelHistory(const Model_t * object);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /** RDFAnnotation_h **/
