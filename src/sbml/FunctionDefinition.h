/**
 * \file    FunctionDefinition.h
 * \brief   SBML FunctionDefinition
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
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
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef FunctionDefinition_h
#define FunctionDefinition_h


#include "common/extern.h"


#ifdef __cplusplus


#include <string>
#include "SBase.h"


class ASTNode;
class SBMLVisitor;


class FunctionDefinition : public SBase
{
public:

  /**
   * Creates a new FunctionDefinition, optionally with its id and math
   * attributes set.
   */
  LIBSBML_EXTERN
  FunctionDefinition (   const std::string& id
                       , ASTNode*           math );
  /**
   * Creates a new FunctionDefinition, optionally with its id and math (via
   * an infix formula string) attributes set.
   */
  LIBSBML_EXTERN
  FunctionDefinition (   const std::string& id      = ""
                       , const std::string& formula = "" );

  /**
   * Destroys this FunctionDefinition.
   */
  LIBSBML_EXTERN
  virtual ~FunctionDefinition ();


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next
   * FunctionDefinition (if available).
   */
  LIBSBML_EXTERN
  bool accept (SBMLVisitor& v) const;

  /**
   * @return the nth argument (bound variable) passed to this
   * FunctionDefinition.
   */
  LIBSBML_EXTERN
  const ASTNode* getArgument (unsigned int n) const;

  /**
   * @return the argument (bound variable) in this FunctionDefinition with
   * the given name or NULL if no such argument exists.
   */
  LIBSBML_EXTERN
  const ASTNode* getArgument (const std::string& name) const;

  /**
   * @return the body of this FunctionDefinition, or NULL if no body is
   * defined.
   */
  LIBSBML_EXTERN
  const ASTNode* getBody () const;

  /**
   * @return the id of this FunctionDefinition.
   */
  LIBSBML_EXTERN
  const std::string& getId () const;

  /**
   * @return the name of this FunctionDefinition.
   */
  LIBSBML_EXTERN
  const std::string& getName () const;

  /**
   * @return the math of this FunctionDefinition.
   */
  LIBSBML_EXTERN
  const ASTNode* getMath () const;

  /**
   * @return the number of arguments (bound variables) that must be passed
   * to this FunctionDefinition.
   */
  LIBSBML_EXTERN
  unsigned int getNumArguments () const;

  /**
   * @return true if the id of this FunctionDefinition has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetId () const;

  /**
   * @return true if the name of this FunctionDefinition has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetName () const;

  /**
   * @return true if the math of this FunctionDefinition has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetMath () const;

  /**
   * Sets the id of this FunctionDefinition to a copy of sid.
   */
  LIBSBML_EXTERN
  void setId (const std::string& sid);

  /**
   * Sets the name of this FunctionDefinition to a copy of string.
   */
  LIBSBML_EXTERN
  void setName (const std::string& str);

  /**
   * Sets the math of this FunctionDefinition to the given ASTNode.
   *
   * The node <b>is not copied</b> and this FunctionDefinition <b>takes
   * ownership</b> of it; i.e. subsequent calls to this function or a call
   * to FunctionDefinition_free() will free the ASTNode (and any child
   * nodes).
   */
  LIBSBML_EXTERN
  void setMath (ASTNode* math);

  /**
   * Unsets the name of this FunctionDefinition.
   */
  LIBSBML_EXTERN
  void unsetName ();


protected:

  std::string id;
  std::string name;
  ASTNode*    math;


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "common/sbmlfwd.h"


/**
 * Creates a new FunctionDefinition and returns a pointer to it.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
FunctionDefinition_create (void);

/**
 * Creates a new FunctionDefinition with the given id and math and returns
 * a pointer to it.  This convenience function is functionally equivalent
 * to:
 *
 *   fd = FunctionDefinition_create();
 *   FunctionDefinition_setId(fd, id); FunctionDefinition_setMath(fd, math);
 */
LIBSBML_EXTERN
FunctionDefinition_t *
FunctionDefinition_createWith (const char *sid, ASTNode_t *math);

/**
 * Frees the given FunctionDefinition.
 */
LIBSBML_EXTERN
void
FunctionDefinition_free (FunctionDefinition_t *fd);


/**
 * @return the nth argument (bound variable) passed to this
 * FunctionDefinition.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getArgument (const FunctionDefinition_t *fd, unsigned int n);

/**
 * @return the argument (bound variable) in this FunctionDefinition with
 * the given name or NULL if no such argument exists.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getArgumentByName ( FunctionDefinition_t *fd,
                                       const char *name );

/**
 * @return the body of this FunctionDefinition, or NULL if no body is
 * defined.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getBody (const FunctionDefinition_t *fd);

/**
 * @return the id of this FunctionDefinition.
 */
LIBSBML_EXTERN
const char *
FunctionDefinition_getId (const FunctionDefinition_t *fd);

/**
 * @return the name of this FunctionDefinition.
 */
LIBSBML_EXTERN
const char *
FunctionDefinition_getName (const FunctionDefinition_t *fd);

/**
 * @return the math of this FunctionDefinition.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getMath (const FunctionDefinition_t *fd);

/**
 * @return the number of arguments (bound variables) that must be passed
 * to this FunctionDefinition.
 */
LIBSBML_EXTERN
unsigned int
FunctionDefinition_getNumArguments (const FunctionDefinition_t *fd);


/**
 * @return 1 if the id of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetId (const FunctionDefinition_t *fd);

/**
 * @return 1 if the name of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetName (const FunctionDefinition_t *fd);

/**
 * @return 1 if the math of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetMath (const FunctionDefinition_t *fd);


/**
 * Sets the id of this FunctionDefinition to a copy of sid.
 */
LIBSBML_EXTERN
void
FunctionDefinition_setId (FunctionDefinition_t *fd, const char *sid);

/**
 * Sets the name of this FunctionDefinition to a copy of string.
 */
LIBSBML_EXTERN
void
FunctionDefinition_setName (FunctionDefinition_t *fd, const char *string);

/**
 * Sets the math of this FunctionDefinition to the given ASTNode.
 *
 * The node <b>is not copied</b> and this FunctionDefinition <b>takes
 * ownership</b> of it; i.e. subsequent calls to this function or a call to
 * FunctionDefinition_free() will free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
FunctionDefinition_setMath (FunctionDefinition_t *fd, ASTNode_t *math);


/**
 * Unsets the name of this FunctionDefinition.  This is equivalent to:
 * safe_free(fd->name); fd->name = NULL;
 */
LIBSBML_EXTERN
void
FunctionDefinition_unsetName (FunctionDefinition_t *fd);


/**
 * The FunctionDefinitionIdCmp function compares the string sid to fd->id.
 *
 * @returns an integer less than, equal to, or greater than zero if sid is
 * found to be, respectively, less than, to match or be greater than
 * fd->id.  Returns -1 if either sid or fd->id is NULL.
 */
LIBSBML_EXTERN
int
FunctionDefinitionIdCmp (const char *sid, const FunctionDefinition_t *fd);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* FunctionDefinition_h */
