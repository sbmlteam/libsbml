/**
 * Filename    : SBMLWriter.cpp
 * Description : Writes an SBML Document to file or in-memory string
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-03-07
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


#include <xercesc/framework/LocalFileFormatTarget.hpp>
#include <xercesc/framework/MemBufFormatTarget.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include "sbml/SBMLFormatter.hpp"
#include "sbml/SBMLWriter.h"


const char *CHARACTER_ENCODING_STRINGS[] =
{
    "ASCII"
  , "UTF-8"
  , "UTF-16"
  , "ISO-8859-1"
  , "(Invalid Character Encoding)"
};


/**
 * @return 1 if the given CharacterEncoding is invalid, 0 otherwise.
 */
unsigned int
CharacterEncoding_isInvalid (CharacterEncoding_t ce)
{
  return
    (ce < CHARACTER_ENCODING_ASCII) || (ce >= CHARACTER_ENCODING_INVALID);
}


/**
 * Returns the string for the given CharacterEncoding.  The caller does not
 * own the returned string and is therefore not allowed to modify it.
 */
const char *
CharacterEncoding_toString (CharacterEncoding_t ce)
{
  if ( CharacterEncoding_isInvalid(ce) )
  {
    ce = CHARACTER_ENCODING_INVALID;
  }

  return CHARACTER_ENCODING_STRINGS[ce];
}


/**
 * Creates a new SBMLWriter and returns a pointer to it.
 *
 * By default the character encoding is UTF-8
 * (CHARACTER_ENCODING_UTF_8).
 */
LIBSBML_EXTERN
SBMLWriter_t *
SBMLWriter_create (void)
{
  SBMLWriter_t *sw = (SBMLWriter_t *) safe_malloc( sizeof(SBMLWriter_t) );


  SBMLWriter_initDefaults(sw);

  return sw;
}


/**
 * Frees the given SBMLWriter.
 */
LIBSBML_EXTERN
void
SBMLWriter_free (SBMLWriter_t *sw)
{
  safe_free(sw);
}


/**
 * Initializes the fields of this SBMLWriter to their defaults:
 *
 *  - encoding = CHARACTER_ENCODING_UTF_8
 */
LIBSBML_EXTERN
void
SBMLWriter_initDefaults (SBMLWriter_t *sw)
{
  sw->encoding = CHARACTER_ENCODING_UTF_8;
}


/**
 * Sets the character encoding for this SBMLWriter to the given
 * CharacterEncoding type.
 */
LIBSBML_EXTERN
void
SBMLWriter_setEncoding (SBMLWriter_t *sw, CharacterEncoding_t encoding)
{
  sw->encoding = encoding;
}


/**
 * Writes the given SBML document to filename (with the settings provided
 * by this SBMLWriter).
 *
 * @return 1 on success and 0 on failure (e.g., if filename could not be
 * opened for writing or the SBMLWriter character encoding is invalid).*
 */
LIBSBML_EXTERN
int
SBMLWriter_writeSBML ( SBMLWriter_t   *sw,
                       SBMLDocument_t *d,
                       const char     *filename )
{
  const char *encoding = NULL;
  int        result    = 0;

  LocalFileFormatTarget* target    = NULL;
  SBMLFormatter*         formatter = NULL;


  try
  {
    if ( !CharacterEncoding_isInvalid(sw->encoding) )
    {
      XMLPlatformUtils::Initialize();

      encoding  = CharacterEncoding_toString(sw->encoding);
      target    = new LocalFileFormatTarget(filename);
      formatter = new SBMLFormatter(encoding, target);

      *formatter << d;
      result = 1;
    }
  }
  catch (...)
  {
    result = 0;
  }


  if (target != NULL)
  {
    delete target;
  }

  if (formatter != NULL)
  {
    delete formatter;
  }

  return result;
}


/**
 * Writes the given SBML document to an in-memory string (with the settings
 * provided by this SBMLWriter) and returns a pointer to it.  The string is
 * owned by the caller and should be freed (with free()) when no longer
 * needed.
 *
 * @return NULL on failure (e.g., if the SBMLWriter character encoding is
 * invalid).
 */
LIBSBML_EXTERN
char *
SBMLWriter_writeSBMLToString (SBMLWriter_t *sw, SBMLDocument_t *d)
{
  char*       result   = NULL;
  const char* encoding = NULL;

  MemBufFormatTarget* target    = NULL;
  SBMLFormatter*      formatter = NULL;


  try
  {
    if ( !CharacterEncoding_isInvalid(sw->encoding) )
    {
      XMLPlatformUtils::Initialize();

      encoding  = CharacterEncoding_toString(sw->encoding);
      target    = new MemBufFormatTarget();
      formatter = new SBMLFormatter(encoding, target);

      *formatter << d;
      result = safe_strdup( (char *) target->getRawBuffer() );
    }
  }
  catch (...)
  {
    safe_free(result);
    result = NULL;
  }


  if (target != NULL)
  {
    delete target;
  }

  if (formatter != NULL)
  {
    delete formatter;
  }

  return result;
}


/**
 * Writes the given SBML document to filename with the settings provided by
 * this SBMLWriter.  This convenience function is functionally equivalent
 * to:
 *
 *   SBMLWriter_writeSBML(SBMLWriter_create(), d, filename);
 *
 * @return 1 on success and 0 on failure (e.g., if filename could not be
 * opened for writing or the SBMLWriter character encoding is invalid).
 */
LIBSBML_EXTERN
int
writeSBML (SBMLDocument_t *d, const char *filename)
{
  SBMLWriter_t *sw = SBMLWriter_create();
  int          result;


  result = SBMLWriter_writeSBML(sw, d, filename);

  SBMLWriter_free(sw);

  return result;
}


/**
 * Writes the given SBML document to an in-memory string (with the settings
 * provided by this SBMLWriter) and returns a pointer to it.  The string is
 * owned by the caller and should be freed (with free()) when no longer
 * needed.  This convenience function is functionally equivalent to:
 *
 *   SBMLWriter_writeSBMLToString(SBMLWriter_create(), d);
 *
 * @return NULL on failure (e.g., if the SBMLWriter character encoding is
 * invalid).
 */
LIBSBML_EXTERN
char *
writeSBMLToString (SBMLDocument_t *d)
{
  SBMLWriter_t sw;


  SBMLWriter_initDefaults(&sw);

  return SBMLWriter_writeSBMLToString(&sw, d);
}
