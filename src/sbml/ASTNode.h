/**
 * Filename    : ASTNode.h
 * Description : Abstract Syntax Tree (AST) for representing formula trees
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2003-05-02
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef ASTNode_h
#define ASTNode_h


#include "common.h"
#include "FormulaTokenizer.h"
#include "List.h"


BEGIN_C_DECLS


/**
 * Nodes of type AST_TRANSIENT are used internally as the AST is being
 * constructed.  Trees returned by SBML_parseFormula() will not contain
 * transient nodes.
 */
typedef enum
{
    AST_FUNCTION  = -1
  , AST_NAME      = TT_NAME
  , AST_INTEGER   = TT_INTEGER
  , AST_REAL      = TT_REAL
  , AST_PLUS      = '+'
  , AST_MINUS     = '-'
  , AST_TIMES     = '*'
  , AST_DIVIDE    = '/'
  , AST_POWER     = '^'
  , AST_TRANSIENT = TT_UNKNOWN
} ASTNodeType_t;


/**
 * An Abstract Syntax tree node has a type (above), a value and a list of
 * child nodes.  The node value is either a character (an operator), a
 * string (for function and variable names) a (long) integer or a real
 * (double) number.
 */
typedef struct
{
  ASTNodeType_t type;

  union
  {
    char   ch;
    char   *name;
    long   integer;
    double real;
  } value;

  List_t *children;
} ASTNode_t;


/**
 * Creates a new ASTNode and returns a pointer to it.  The returned node
 * will have a type of AST_TRANSIENT and should be set to something else as
 * soon as possible.
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_create (void);

/**
 * Creates a new ASTNode from the given Token and returns a pointer to it.
 * The returned ASTNode will contain the same data as the Token.
 */
ASTNode_t *
ASTNode_createFromToken (Token_t *token);

/**
 * Frees the given ASTNode including any child nodes.
 */
LIBSBML_EXTERN
void
ASTNode_free (ASTNode_t *node);

/**
 * Frees the name of this ASTNode and sets it to NULL, if the node type is
 * either AST_FUNCTION or AST_NAME.
 */
void
ASTNode_freeName (ASTNode_t *node);

/**
 * Adds the given node as a child of this ASTNode.  Child nodes are added
 * in-order from "left-to-right".
 */
LIBSBML_EXTERN
void
ASTNode_addChild (ASTNode_t *node, ASTNode_t *child);

/**
 * @return the nth child of this ASTNode or NULL if this node has no nth
 * child (n > ASTNode_getNumChildren() - 1).
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_getChild (const ASTNode_t *node, unsigned int n);

/**
 * @return the left child of this ASTNode.  This is equivalent to
 * ASTNode_getChild(node, 0);
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_getLeftChild (const ASTNode_t *node);

/**
 * @return the right child of this ASTNode or NULL if this node has no
 * right child.  If ASTNode_getNumChildren(node) > 1, then this is
 * equivalent to:
 *
 *   ASTNode_getChild(node, ASTNode_getNumChildren(node) - 1);
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_getRightChild (const ASTNode_t *node);

/**
 * @return the number of children of this ASTNode or 0 is this node has no
 * children.
 */
LIBSBML_EXTERN
unsigned int
ASTNode_getNumChildren (const ASTNode_t *node);

/**
 * @return the value of this ASTNode as a single character.  This function
 * should be called only when ASTNode_getType() is one of AST_PLUS,
 * AST_MINUS, AST_TIMES, AST_DIVIDE or AST_POWER.
 */
LIBSBML_EXTERN
char
ASTNode_getCharacter (const ASTNode_t *node);

/**
 * @return the value of this ASTNode as a (long) integer.  This function
 * should be called only when ASTNode_getType() == AST_INTEGER.
 */
LIBSBML_EXTERN
long
ASTNode_getInteger (const ASTNode_t *node);

/**
 * @return the value of this ASTNode as a string.  This function should be
 * called only when ASTNode_getType() is either AST_FUNCTION or AST_NAME.
 */
LIBSBML_EXTERN
const char *
ASTNode_getName (const ASTNode_t *node);

/**
 * @return the value of this ASTNode as a real (double).  This function
 * should be called only when ASTNode_getType() == AST_REAL.
 */
LIBSBML_EXTERN
double
ASTNode_getReal (const ASTNode_t *node);

/**
 * @return the type of this ASTNode.
 */
LIBSBML_EXTERN
ASTNodeType_t
ASTNode_getType (const ASTNode_t *node);

/**
 * Sets the value of this ASTNode to the given character.  If character is
 * one of '+', '-', '*', '/' or '^', the node type will be set accordingly.
 * For all other characters, the node type will be set to AST_TRANSIENT.
 */
LIBSBML_EXTERN
void
ASTNode_setCharacter (ASTNode_t *node, char value);

/**
 * Sets the value of this ASTNode to the given name and sets the node type
 * to AST_NAME.
 */
LIBSBML_EXTERN
void
ASTNode_setName (ASTNode_t *node, const char *name);

/**
 * Sets the value of this ASTNode to the given (long) integer and sets the
 * node type to AST_INTEGER.
 */
LIBSBML_EXTERN
void
ASTNode_setInteger (ASTNode_t *node, long value);

/**
 * Sets the value of this ASTNode to the given real (double) and sets the
 * node type to AST_REAL.
 */
LIBSBML_EXTERN
void
ASTNode_setReal (ASTNode_t *node, double value);

/**
 * Sets the type of this ASTNode to the given ASTNodeType.
 */
LIBSBML_EXTERN
void
ASTNode_setType (ASTNode_t *node, ASTNodeType_t type);


END_C_DECLS


#endif  /** ASTNode_h **/
