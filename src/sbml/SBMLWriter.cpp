/**
 * \file    SBMLWriter.cpp
 * \brief   Writes an SBML Document to file or in-memory string
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
 *   Stefan Hoops
 */


#include "common/common.h"


#ifdef USE_EXPAT
#  include "xml/ExpatFormatter.h"
#else
#  include <xercesc/framework/LocalFileFormatTarget.hpp>
#  include <xercesc/framework/MemBufFormatTarget.hpp>
#  include <xercesc/util/PlatformUtils.hpp>
#endif  // !USE_EXPAT


#include "xml/StreamFormatTarget.h"
#include "SBMLFormatter.h"
#include "SBMLWriter.h"


using namespace std;


/**
 * Creates a new SBMLWriter.
 */
LIBSBML_EXTERN
SBMLWriter::SBMLWriter ()
{
}


/**
 * Destroys this SBMLWriter.
 */
LIBSBML_EXTERN
SBMLWriter::~SBMLWriter ()
{
}


/**
 * Sets the name of this program, i.e. the one about to write out the
 * SBMLDocument.  If the program name and version are set
 * (setProgramVersion()), the following XML comment, intended for human
 * consumption, will be written at the beginning of the document:
 *
 *   <!-- Created by <program name> version <program version>
 *   on yyyy-MM-dd HH:mm with libsbml version <libsbml version>. -->
 */
LIBSBML_EXTERN
void
SBMLWriter::setProgramName (const string& name)
{
  mProgramName = name;
}


/**
 * Sets the version of this program, i.e. the one about to write out the
 * SBMLDocument.  If the program version and name are set
 * (setProgramName()), the following XML comment, intended for human
 * consumption, will be written at the beginning of the document:
 *
 *   <!-- Created by <program name> version <program version>
 *   on yyyy-MM-dd HH:mm with libsbml version <libsbml version>. -->
 */
LIBSBML_EXTERN
void
SBMLWriter::setProgramVersion (const string& version)
{
  mProgramVersion = version;
}


/**
 * Writes the given SBML document to filename.
 *
 * @return true on success and false if the filename could not be opened
 * for writing.
 */
LIBSBML_EXTERN
bool
SBMLWriter::write (const SBMLDocument& d, const string& filename)
{
  bool result = false;

  LocalFileFormatTarget* target    = 0;
  SBMLFormatter*         formatter = 0;


  try
  {
#ifndef USE_EXPAT
    XMLPlatformUtils::Initialize();
#endif  // !USE_EXPAT

    target    = new LocalFileFormatTarget(filename.c_str());
    formatter = new SBMLFormatter(target);

    if (!mProgramName.empty() && !mProgramVersion.empty())
    {
      formatter->writeComment(mProgramName, mProgramVersion);
    }

    *formatter << d;
    result = true;
  }
  catch (...)
  {
  }

  delete target;
  delete formatter;

  return result;
}


/**
 * Writes the given SBML document to the output stream.
 *
 * @return true on success and false if one of the underlying Xerces or
 * Expat components fail (rare).
 */
LIBSBML_EXTERN
bool
SBMLWriter::write (const SBMLDocument& d, ostream& stream)
{
  bool result = false;

  StreamFormatTarget* target    = 0;
  SBMLFormatter*      formatter = 0;


  try
  {
#ifndef USE_EXPAT
    XMLPlatformUtils::Initialize();
#endif  // !USE_EXPAT

    target    = new StreamFormatTarget(stream);
    formatter = new SBMLFormatter(target);

    *formatter << d;
    result = true;
  }
  catch (...)
  {
  }

  delete target;
  delete formatter;

  return result;
}


/**
 * Writes the given SBML document to an in-memory string and returns a
 * pointer to it.  The string is owned by the caller and should be freed
 * (with free()) when no longer needed.
 *
 * @return the string on success and 0 if one of the underlying Xerces or
 * Expat components fail (rare).
 */
LIBSBML_EXTERN
char*
SBMLWriter::writeToString (const SBMLDocument& d)
{
  char* result = 0;

  MemBufFormatTarget* target    = 0;
  SBMLFormatter*      formatter = 0;


  try
  {
#ifndef USE_EXPAT
    XMLPlatformUtils::Initialize();
#endif  // !USE_EXPAT

    target    = new MemBufFormatTarget();
    formatter = new SBMLFormatter(target);

    *formatter << d;
    result = safe_strdup( (char *) target->getRawBuffer() );
  }
  catch (...)
  {
    safe_free(result);
    result = 0;
  }

  delete target;
  delete formatter;

  return result;
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
  return new(nothrow) SBMLWriter;
}


/**
 * Frees the given SBMLWriter.
 */
LIBSBML_EXTERN
void
SBMLWriter_free (SBMLWriter_t *sw)
{
  delete sw;
}


/**
 * Sets the name of this program, i.e. the one about to write out the
 * SBMLDocument.  If the program name and version are set
 * (setProgramVersion()), the following XML comment, intended for human
 * consumption, will be written at the beginning of the document:
 *
 *   <!-- Created by <program name> version <program version>
 *   on yyyy-MM-dd HH:mm with libsbml version <libsbml version>. -->
 */
LIBSBML_EXTERN
void
SBMLWriter_setProgramName (SBMLWriter_t *sw, const char *name)
{
  sw->setProgramName(name);
}


/**
 * Sets the version of this program, i.e. the one about to write out the
 * SBMLDocument.  If the program version and name are set
 * (setProgramName()), the following XML comment, intended for human
 * consumption, will be written at the beginning of the document:
 *
 *   <!-- Created by <program name> version <program version>
 *   on yyyy-MM-dd HH:mm with libsbml version <libsbml version>. -->
 */
LIBSBML_EXTERN
void
SBMLWriter_setProgramVersion (SBMLWriter_t *sw, const char *version)
{
  sw->setProgramVersion(version);
}


/**
 * Writes the given SBML document to filename.
 *
 * @return non-zero on success and zero if the filename could not be opened
 * for writing.)
 */
LIBSBML_EXTERN
int
SBMLWriter_writeSBML ( SBMLWriter_t         *sw,
                       const SBMLDocument_t *d,
                       const char           *filename )
{
  return sw->write(*d, filename);
}


/**
 * Writes the given SBML document to an in-memory string and returns a
 * pointer to it.  The string is owned by the caller and should be freed
 * (with free()) when no longer needed.
 *
 * @return the string on success and NULL if one of the underlying Xerces or
 * Expat components fail (rare).
 */
LIBSBML_EXTERN
char *
SBMLWriter_writeSBMLToString (SBMLWriter_t *sw, const SBMLDocument_t *d)
{
  return sw->writeToString(*d);
}


/**
 * Writes the given SBML document to filename.  This convenience function
 * is functionally equivalent to:
 *
 *   SBMLWriter_writeSBML(SBMLWriter_create(), d, filename);
 *
 * @return non-zero on success and zero if the filename could not be opened
 * for writing.)
 */
LIBSBML_EXTERN
int
writeSBML (const SBMLDocument_t *d, const char *filename)
{
  SBMLWriter sw;
  return sw.write(*d, filename);
}


/**
 * Writes the given SBML document to an in-memory string and returns a
 * pointer to it.  The string is owned by the caller and should be freed
 * (with free()) when no longer needed.  This convenience function is
 * functionally equivalent to:
 *
 *   SBMLWriter_writeSBMLToString(SBMLWriter_create(), d);
 *
 * @return the string on success and NULL if one of the underlying Xerces
 * or Expat components fail (rare).
 */
LIBSBML_EXTERN
char *
writeSBMLToString (const SBMLDocument_t *d)
{
  SBMLWriter sw;
  return sw.writeToString(*d);
}
