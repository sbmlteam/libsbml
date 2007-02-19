/**
 * \file    RDFAnnotation.h
 * \brief   RDFAnnotation I/O
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
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





/**
 * takes an annotation that has been read into the model
 * identifies the RDF elements
 * and creates a List of CVTerms from the annotation
 * and creates a Model History from the annotation
 */
LIBSBML_EXTERN
void parseRDFAnnotation(XMLNode * annotation, List * CVTerms);


/**
 * takes an annotation that has been read into the model
 * identifies the RDF elements
 * and creates a Model History from the annotation
 */
LIBSBML_EXTERN
ModelHistory* parseRDFAnnotation(XMLNode * annotation);


LIBSBML_EXTERN
XMLNode *
deleteRDFAnnotation(XMLNode * annotation);

/**
 * takes a List of CVTerms
 * and creates the RDF annotation
 */
LIBSBML_EXTERN
XMLNode * parseCVTerms(const SBase * );

/**
 * takes a Model creator information
 * and creates the RDF annotation
 */
LIBSBML_EXTERN
XMLNode * parseModelHistory(const SBase * );


#endif  /* __cplusplus */

#endif  /** RDFAnnotation_h **/
