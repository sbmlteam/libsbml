/**
 * Filename    : FunctionDefinition.cpp
 * Description : SBML FunctionDefinition
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
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
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/FormulaParser.h"

#include "sbml/FunctionDefinition.h"
#include "sbml/FunctionDefinition.hpp"


/**
 * Creates a new FunctionDefinition, optionally with its id and math
 * attributes set.
 */
LIBSBML_EXTERN
FunctionDefinition::FunctionDefinition (   const std::string& id
                                         , ASTNode*           math  ) :
    SBase()
  , id   ( id   )
  , math ( math )
{
  init(SBML_FUNCTION_DEFINITION);
}


/**
 * Creates a new FunctionDefinition, optionally with its id and math (via
 * an infix formula string) attributes set.
 */
LIBSBML_EXTERN
FunctionDefinition::FunctionDefinition (   const std::string& id
                                         , const std::string& formula ) :
    SBase()
  , id   ( id   )
  , math ( NULL )
{
  init(SBML_FUNCTION_DEFINITION);

  if ( !formula.empty() )
  {
    setMath( (ASTNode*) SBML_parseFormula( formula.c_str() ) );
  }
}


/**
 * Destroys this FunctionDefinition.
 */
LIBSBML_EXTERN
FunctionDefinition::~FunctionDefinition ()
{
  delete math;
}


/**
 * @return the id of this FunctionDefinition.
 */
LIBSBML_EXTERN
const std::string& 
FunctionDefinition::getId () const
{
  return id;
}


/**
 * @return the name of this FunctionDefinition.
 */
LIBSBML_EXTERN
const std::string&
FunctionDefinition::getName () const
{
  return name;
}


/**
 * @return the math of this FunctionDefinition.
 */
LIBSBML_EXTERN
const ASTNode*
FunctionDefinition::getMath () const
{
  return math;
}


/**
 * @return true if the id of this FunctionDefinition has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
FunctionDefinition::isSetId () const
{
  return ! id.empty();
}


/**
 * @return true if the name of this FunctionDefinition has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
FunctionDefinition::isSetName () const
{
  return ! name.empty();
}


/**
 * @return true if the math of this FunctionDefinition has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
FunctionDefinition::isSetMath () const
{
  return (math != NULL);
}


/**
 * Sets the id of this FunctionDefinition to a copy of sid.
 */
LIBSBML_EXTERN
void
FunctionDefinition::setId (const std::string& sid)
{
  id = sid;
}


/**
 * Sets the name of this FunctionDefinition to a copy of string.
 */
LIBSBML_EXTERN
void
FunctionDefinition::setName (const std::string& string)
{
  name = string;
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
FunctionDefinition::setMath (ASTNode* math)
{
  if (this->math == math) return;



  delete this->math;
  this->math = math;
}


/**
 * Unsets the name of this FunctionDefinition.
 */
LIBSBML_EXTERN
void
FunctionDefinition::unsetName ()
{
  name.erase();
}




/**
 * Creates a new FunctionDefinition and returns a pointer to it.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
FunctionDefinition_create (void)
{
  return new(std::nothrow) FunctionDefinition("", "");
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
FunctionDefinition_createWith (const char *sid, ASTNode_t *math)
{
  ASTNode* x = static_cast<ASTNode*>(math);


  return
    new(std::nothrow) FunctionDefinition(sid ? sid : "", x);
}


/**
 * Frees the given FunctionDefinition.
 */
LIBSBML_EXTERN
void
FunctionDefinition_free (FunctionDefinition_t *fd)
{
  delete static_cast<FunctionDefinition*>(fd);
}


/**
 * @return the id of this FunctionDefinition.
 */
LIBSBML_EXTERN
const char *
FunctionDefinition_getId (const FunctionDefinition_t *fd)
{
  const FunctionDefinition* x = static_cast<const FunctionDefinition*>(fd);


  return x->isSetId() ? x->getId().c_str() : NULL;
}


/**
 * @return the name of this FunctionDefinition.
 */
LIBSBML_EXTERN
const char *
FunctionDefinition_getName (const FunctionDefinition_t *fd)
{
  const FunctionDefinition* x = static_cast<const FunctionDefinition*>(fd);


  return x->isSetName() ? x->getName().c_str() : NULL;
}


/**
 * @return the math of this FunctionDefinition.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getMath (const FunctionDefinition_t *fd)
{
  return static_cast<const FunctionDefinition*>(fd)->getMath();
}


/**
 * @return 1 if the id of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetId (const FunctionDefinition_t *fd)
{
  return (int) static_cast<const FunctionDefinition*>(fd)->isSetId();
}


/**
 * @return 1 if the name of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetName (const FunctionDefinition_t *fd)
{
  return (int) static_cast<const FunctionDefinition*>(fd)->isSetName();
}


/**
 * @return 1 if the math of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetMath (const FunctionDefinition_t *fd)
{
  return (int) static_cast<const FunctionDefinition*>(fd)->isSetMath();
}


/**
 * Sets the id of this FunctionDefinition to a copy of sid.
 */
LIBSBML_EXTERN
void
FunctionDefinition_setId (FunctionDefinition_t *fd, const char *sid)
{
  static_cast<FunctionDefinition*>(fd)->setId(sid ? sid : "");
}


/**
 * Sets the name of this FunctionDefinition to a copy of string.
 */
LIBSBML_EXTERN
void
FunctionDefinition_setName (FunctionDefinition_t *fd, const char *string)
{
  if (string == NULL)
  {
    static_cast<FunctionDefinition*>(fd)->unsetName();
  }
  else
  {
    static_cast<FunctionDefinition*>(fd)->setName(string);
  }
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
  static_cast<FunctionDefinition*>(fd)->setMath( static_cast<ASTNode*>(math) );
}


/**
 * Unsets the name of this FunctionDefinition.  This is equivalent to:
 * safe_free(fd->name); fd->name = NULL;
 */
LIBSBML_EXTERN
void
FunctionDefinition_unsetName (FunctionDefinition_t *fd)
{
  static_cast<FunctionDefinition*>(fd)->unsetName();
}


/**
 * The FunctionDefinitionIdCmp function compares the string sid to fd->id.
 *
 * @returns an integer less than, equal to, or greater than zero if sid is
 * found to be, respectively, less than, to match or be greater than
 * fd->id.  Returns -1 if either sid or fd->id is NULL.
 */
LIBSBML_EXTERN
int
FunctionDefinitionIdCmp (const char *sid, const FunctionDefinition_t *fd)
{
  int result = -1;


  if (sid != NULL && FunctionDefinition_isSetId(fd))
  {
    result = strcmp(sid, FunctionDefinition_getId(fd));
  }

  return result;
}
