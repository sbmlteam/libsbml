/**
 * Filename    : ParseMessage.cpp
 * Description : Stores error message encountered during an SBML parse
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-04-16
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


#include "sbml/ParseMessage.h"
#include "sbml/ParseMessage.hpp"


/**
 * Creates a new ParseMessage reporting that message occurred at the given
 * line and column.  Each ParseMessage has an identification number
 * associated with it.
 */
LIBSBML_EXTERN
ParseMessage::ParseMessage (   unsigned int       id
                             , const std::string& message
                             , unsigned int       line
                             , unsigned int       column) :
    mId     ( id      )
  , mMessage( message )
  , mLine   ( line    )
  , mColumn ( column  )
{
}


/**
 * Destroys this ParseMessage.
 */
ParseMessage::~ParseMessage ()
{
}


/**
 * @return the id of this ParseMessage.
 */
LIBSBML_EXTERN
unsigned int
ParseMessage::getId () const
{
  return mId;
}


/**
 * @return the message text of this ParseMessage.
 */
LIBSBML_EXTERN
const std::string&
ParseMessage::getMessage () const
{
  return mMessage;
}


/**
 * @return the line number where this ParseMessage ocurred.
 */
LIBSBML_EXTERN
unsigned int
ParseMessage::getLine () const
{
  return mLine;
}


/**
 * @return the column number where this ParseMessage occurred.
 */
LIBSBML_EXTERN
unsigned int
ParseMessage::getColumn () const
{
  return mColumn;
}


/**
 * Creates a new ParseMessage and returns a pointer to it.
 */
ParseMessage_t *
ParseMessage_create (void)
{
  return new ParseMessage;
}



/**
 * Creates a new ParseMessage reporting that message occurred at the given
 * line and column.  Each ParseMessage has an identification number
 * associated with it.
 */
ParseMessage_t *
ParseMessage_createWith (   unsigned int  id
                          , const char   *message
                          , unsigned int  line
                          , unsigned int  column )
{
  return new ParseMessage(id, message, line, column);
}


/**
 * Frees the given ParseMessage.
 */
void
ParseMessage_free (ParseMessage_t *pm)
{
  delete static_cast<ParseMessage*>(pm);
}


/**
 * @return the id of this ParseMessage.
 */
LIBSBML_EXTERN
unsigned int
ParseMessage_getId (const ParseMessage_t *pm)
{
  return static_cast<const ParseMessage*>(pm)->getId();
}


/**
 * @return the message text of this ParseMessage.
 */
LIBSBML_EXTERN
const char *
ParseMessage_getMessage (const ParseMessage_t *pm)
{
  const std::string& s = static_cast<const ParseMessage*>(pm)->getMessage();


  return s.empty() ? NULL : s.c_str();
}


/**
 * @return the line number where this ParseMessage ocurred.
 */
LIBSBML_EXTERN
unsigned int
ParseMessage_getLine (const ParseMessage_t *pm)
{
  return static_cast<const ParseMessage*>(pm)->getLine();
}


/**
 * @return the column number where this ParseMessage occurred.
 */
LIBSBML_EXTERN
unsigned int
ParseMessage_getColumn (const ParseMessage_t *pm)
{
  return static_cast<const ParseMessage*>(pm)->getColumn();
}
