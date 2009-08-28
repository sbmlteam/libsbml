/**
 * @file    MathML.h
 * @brief   Utilities for reading and writing MathML to/from text strings.
 * @author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2009 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#ifndef MathML_h
#define MathML_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus

#include <limits>
#include <iomanip>
#include <string>
#include <sstream>

#include <cstdlib>

LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygen-libsbml-internal */

class ASTNode;
class XMLInputStream;
class XMLOutputStream;


/**
 * Reads the MathML from the given XMLInputStream, constructs a corresponding
 * abstract syntax tree and returns a pointer to the root of the tree.
 */
LIBSBML_EXTERN
ASTNode*
readMathML (XMLInputStream& stream);


/**
 * Writes the given ASTNode (and its children) to the XMLOutputStream as
 * MathML.
 */
LIBSBML_EXTERN
void
writeMathML (const ASTNode* node, XMLOutputStream& stream);


/** @endcond doxygen-libsbml-internal */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


/**
 * Reads the MathML from the given XML string, constructs a corresponding
 * abstract syntax tree, and returns a pointer to the root of the tree.
 *
 * @param xml a string containing a full MathML expression
 *
 * @return the root of an AST corresponding to the given mathematical
 * expression, otherwise NULL is returned if the given string is NULL
 * or invalid.
 */
LIBSBML_EXTERN
ASTNode_t *
readMathMLFromString (const char *xml);


/**
 * Writes the given ASTNode (and its children) to a string as MathML, and
 * returns the string.
 *
 * @param node the root of an AST to write out to the stream.
 *
 * @return a string containing the written-out MathML representation
 * of the given AST
 *
 * @note The string is owned by the caller and should be freed (with
 * free()) when no longer needed. NULL is returned if the given argument
 * is NULL.
 */
LIBSBML_EXTERN
char *
writeMathMLToString (const ASTNode_t* node);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /** MathML_h **/
