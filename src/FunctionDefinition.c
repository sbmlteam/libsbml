/**
 * Filename    : FunctionDefinition.c
 * Description : SBML FunctionDefinition
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-05-03
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
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/FunctionDefinition.h"


/**
 * Creates a new FunctionDefinition and returns a pointer to it.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
FunctionDefinition_create (void)
{
  FunctionDefinition_t *fd;


  fd = (FunctionDefinition_t *) safe_calloc(1, sizeof(FunctionDefinition_t));
  SBase_init((SBase_t *) fd, SBML_FUNCTION_DEFINITION);

  return fd;
}


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
FunctionDefinition_createWith (const char *sid, ASTNode_t* math)
{
  FunctionDefinition_t *fd = FunctionDefinition_create();


  FunctionDefinition_setId  ( fd, sid  );
  FunctionDefinition_setMath( fd, math );

  return fd;
}


/**
 * Frees the given FunctionDefinition.
 */
LIBSBML_EXTERN
void
FunctionDefinition_free (FunctionDefinition_t *fd)
{
  if (fd == NULL) return;


  SBase_clear((SBase_t *) fd);

  safe_free(fd->id);
  safe_free(fd->name);

  ASTNode_free(fd->math);

  safe_free(fd);
}


/**
 * @return the id of this FunctionDefinition.
 */
LIBSBML_EXTERN
const char *
FunctionDefinition_getId (const FunctionDefinition_t *fd)
{
  return fd->id;
}


/**
 * @return the name of this FunctionDefinition.
 */
LIBSBML_EXTERN
const char *
FunctionDefinition_getName (const FunctionDefinition_t *fd)
{
  return fd->name;
}


/**
 * @return the math of this FunctionDefinition.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getMath (const FunctionDefinition_t *fd)
{
  return fd->math;
}


/**
 * @return 1 if the id of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetId (const FunctionDefinition_t *fd)
{
  return (fd->id != NULL);
}


/**
 * @return 1 if the name of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetName (const FunctionDefinition_t *fd)
{
  return (fd->name != NULL);
}


/**
 * @return 1 if the math of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetMath (const FunctionDefinition_t *fd)
{
  return (fd->math != NULL);
}


/**
 * Sets the id of this FunctionDefinition to a copy of sid.
 */
LIBSBML_EXTERN
void
FunctionDefinition_setId (FunctionDefinition_t *fd, const char *sid)
{
  if (fd->id == sid) return;


  if (fd->id != NULL)
  {
    safe_free(fd->id);
  }

  fd->id = (sid == NULL) ? NULL : safe_strdup(sid);
}


/**
 * Sets the name of this FunctionDefinition to a copy of string.
 */
LIBSBML_EXTERN
void
FunctionDefinition_setName (FunctionDefinition_t *fd, const char *string)
{
  if (fd->name == string) return;


  if (fd->name != NULL)
  {
    safe_free(fd->name);
  }

  fd->name = (string == NULL) ? NULL : safe_strdup(string);
}


/**
 * Sets the math of this FunctionDefinition to the given ASTNode.
 *
 * The node <b>is not copied</b> and this FunctionDefinition <b>takes
 * ownership</b> of it; i.e. subsequent calls to this function or a call to
 * FunctionDefinition_free() will free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
FunctionDefinition_setMath (FunctionDefinition_t *fd, ASTNode_t *math)
{
  if (fd->math == math) return;


  if (fd->math != NULL)
  {
    ASTNode_free(fd->math);
  }

  fd->math = math;
}


/**
 * Unsets the name of this FunctionDefinition.  This is equivalent to:
 * safe_free(fd->name); fd->name = NULL;
 */
LIBSBML_EXTERN
void
FunctionDefinition_unsetName (FunctionDefinition_t *fd)
{
  safe_free(fd->name);
  fd->name = NULL;
}
