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


#ifndef LayoutAnnotation_h
#define LayoutAnnotation_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>

#include <sbml/xml/XMLAttributes.h>


#ifdef __cplusplus

#ifdef USE_LAYOUT // make the functions inaccesible when the layout is not used


/**
 * takes an annotation that has been read into the model
 * identifies the RDF elements
 * and creates a List of Layouts from the annotation
 */
LIBSBML_EXTERN
void parseLayoutAnnotation(XMLNode * annotation, ListOfLayouts& layouts);

/**
 * Takes an XMLNode and tries to find the layout annotation node and deletes it if it was found.
 */
LIBSBML_EXTERN
XMLNode* deleteLayoutAnnotation(XMLNode* pAnnotation);

/**
 * Creates an XMLNode that represents the layouts of the model from the given Model object.
 */
 LIBSBML_EXTERN
 XMLNode* parseLayouts(const Model* pModel);

#endif // USE_LAOYUT

#endif  /* __cplusplus */

#endif  /** LayoutAnnotation_h **/
