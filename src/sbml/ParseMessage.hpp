/**
 * Filename    : ParseMessage.hpp
 * Description : Stores error message encountered during an SBML parse
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-04-16
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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


#ifndef ParseMessage_hpp
#define ParseMessage_hpp


#include <iosfwd>
#include <string>


class ParseMessage
{
public:

  /**
   * Creates a new ParseMessage reporting that message occurred at the given
   * line and column.  Each ParseMessage has an identification number
   * associated with it.
   */
  LIBSBML_EXTERN
  ParseMessage
  (
      unsigned int       id      = 0
    , const std::string& message = ""
    , unsigned int       line    = 0
    , unsigned int       col     = 0
  );

  /**
   * Destroys this ParseMessage.
   */
  virtual ~ParseMessage ();


  /**
   * @return the id of this ParseMessage.
   */
  LIBSBML_EXTERN
  unsigned int getId () const;

  /**
   * @return the message text of this ParseMessage.
   */
  LIBSBML_EXTERN
  const std::string& getMessage () const;

  /**
   * @return the line number where this ParseMessage ocurred.
   */
  LIBSBML_EXTERN
  unsigned int getLine () const;

  /**
   * @return the column number where this ParseMessage occurred.
   */
  LIBSBML_EXTERN
  unsigned int getColumn () const;

  /**
   * Outputs this ParseMessage to stream in the following format (and
   * followed by a newline):
   *
   *   line:col:(id) message
   */
  friend
  std::ostream& operator<< (std::ostream& stream, const ParseMessage& pm);


protected:

  unsigned int mId;

  std::string  mMessage;

  unsigned int mLine;
  unsigned int mColumn;
};


#endif  // ParseMessage_hpp
