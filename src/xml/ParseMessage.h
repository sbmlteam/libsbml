/**
 * \file    ParseMessage.h
 * \brief   Stores error message encountered during an SBML parse
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
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


#ifndef ParseMessage_h
#define ParseMessage_h


#include "../common/extern.h"


#ifdef __cplusplus


#include <iosfwd>
#include <string>


class ParseMessage
{
public:

  /**
   * Creates a new ParseMessage reporting that message occurred at the
   * given line and column.  Each ParseMessage also has an identification
   * number, a category, and a severity level associated with it.
   */
  LIBSBML_EXTERN
  ParseMessage
  (
      unsigned int        id       = 0
    , const std::string&  message  = ""
    , unsigned int        line     = 0
    , unsigned int        col      = 0
    , unsigned int        severity = 0
    , const std::string&  category = ""
  );

  /**
   * Creates a new ParseMessage by copying an existing ParseMessage.
   */
  LIBSBML_EXTERN
  ParseMessage (const ParseMessage& msg);

  /**
   * Destroys this ParseMessage.
   */
  LIBSBML_EXTERN
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
  const std::string getMessage () const;

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
   * @return the severity of this ParseMessage.  ParseMessages severity
   * levels correspond to those defined in the XML specification (with the
   * addition of Info for informational messages).
   *
   *   0 - Info
   *   1 - Warning
   *   2 - Error
   *   3 - Fatal
   */
  LIBSBML_EXTERN
  unsigned int getSeverity () const;

  /**
   * @return the category of this ParseMessage.  A category is a string,
   * similiar in spirit to an XML namespace, which partitions error
   * messages to prevent id conflicts.  Example categories include:
   *
   *   http://sbml.org/validator/consistency
   *   http://sbml.org/validator/consistency/units
   *   http://sbml.org/validator/compatibility/L1
   */
  LIBSBML_EXTERN
  const std::string getCategory () const;


#ifndef SWIG

  /**
   * Outputs this ParseMessage to stream in the following format (and
   * followed by a newline):
   *
   *   line: (id) message
   */
  friend
  std::ostream& operator<< (std::ostream& stream, const ParseMessage& pm);

#endif  /* !SWIG */


protected:

  unsigned int mId;
  std::string  mMessage;
  unsigned int mLine;
  unsigned int mColumn;
  unsigned int mSeverity;
  std::string  mCategory;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include <stdio.h>
#include "common/sbmlfwd.h"


/**
 * Creates a new ParseMessage and returns a pointer to it.
 */
ParseMessage_t *
ParseMessage_create (void);

/**
 * Creates a new ParseMessage reporting that message occurred at the
 * given line and column.  Each ParseMessage also has an identification
 * number, a category, and a severity level associated with it.
 */
ParseMessage_t *
ParseMessage_createWith (  unsigned int  id
                         , const char    *message
                         , unsigned int  line
                         , unsigned int  column
                         , unsigned int  severity
                         , const char    *category );

/**
 * Frees the given ParseMessage.
 */
void
ParseMessage_free (ParseMessage_t *pm);

/**
 * @return the message text of this ParseMessage.
 */
LIBSBML_EXTERN
const char *
ParseMessage_getMessage (const ParseMessage_t *pm);

/**
 * @return the id of this ParseMessage.
 */
LIBSBML_EXTERN
unsigned int
ParseMessage_getId (const ParseMessage_t *pm);

/**
 * @return the line number where this ParseMessage ocurred.
 */
LIBSBML_EXTERN
unsigned int
ParseMessage_getLine (const ParseMessage_t *pm);

/**
 * @return the column number where this ParseMessage occurred.
 */
LIBSBML_EXTERN
unsigned int
ParseMessage_getColumn (const ParseMessage_t *pm);

/**
 * @return the severity of this ParseMessage.  ParseMessages severity
 * levels correspond to those defined in the XML specification (with the
 * addition of Info for informational messages).
 *
 *   0 - Info
 *   1 - Warning
 *   2 - Error
 *   3 - Fatal
 */
LIBSBML_EXTERN
unsigned int
ParseMessage_getSeverity (const ParseMessage_t *pm);

/**
 * @return the category of this ParseMessage.  A category is a string,
 * similiar in spirit to an XML namespace, which partitions error
 * messages to prevent id conflicts.  Example categories include:
 *
 *   http://sbml.org/validator/consistency
 *   http://sbml.org/validator/consistency/units
 *   http://sbml.org/validator/compatibility/L1
 */
LIBSBML_EXTERN
const char *
ParseMessage_getCategory (const ParseMessage_t *pm);

/**
 * Outputs this ParseMessage to stream in the following format (and
 * followed by a newline):
 *
 *   line: (id) message
 */
LIBSBML_EXTERN
void
ParseMessage_print (const ParseMessage_t *pm, FILE *stream);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* ParseMessage_h */
