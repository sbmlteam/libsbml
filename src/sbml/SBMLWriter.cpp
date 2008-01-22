/**
 * @file    SBMLWriter.cpp
 * @brief   Writes an SBML Document to file or in-memory string
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <ios>
#include <iostream>
#include <fstream>
#include <sstream>

#include <sbml/common/common.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/SBMLError.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLWriter.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/*
 * Creates a new SBMLWriter.
 */
SBMLWriter::SBMLWriter ()
{
}


/*
 * Destroys this SBMLWriter.
 */
SBMLWriter::~SBMLWriter ()
{
}


/*
 * Sets the name of this program. i.\ e.\ the one about to write out the
 * SBMLDocument.  If the program name and version are set
 * (setProgramVersion()), the following XML comment, intended for human
 * consumption, will be written at the beginning of the document:
 *
 *   <!-- Created by <program name> version <program version>
 *   on yyyy-MM-dd HH:mm with libsbml version <libsbml version>. -->
 */
void
SBMLWriter::setProgramName (const std::string& name)
{
  mProgramName = name;
}


/*
 * Sets the version of this program. i.\ e.\ the one about to write out the
 * SBMLDocument.  If the program version and name are set
 * (setProgramName()), the following XML comment, intended for human
 * consumption, will be written at the beginning of the document:
 *
 *   <!-- Created by <program name> version <program version>
 *   on yyyy-MM-dd HH:mm with libsbml version <libsbml version>. -->
 */
void
SBMLWriter::setProgramVersion (const std::string& version)
{
  mProgramVersion = version;
}


/*
 * Writes the given SBML document to filename.
 *
 * @return true on success and false if the filename could not be opened
 * for writing.
 */
bool
SBMLWriter::writeSBML (const SBMLDocument* d, const std::string& filename)
{
  ofstream stream( filename.c_str() );
  
  if (stream.fail() || stream.bad())
  {
    SBMLErrorLog *log = (const_cast<SBMLDocument *>(d))->getErrorLog();
    log->logError(XMLFileUnwritable);
    return false;
  }
  
  return writeSBML(d, stream);
}


/*
 * Writes the given SBML document to the output stream.
 *
 * @return true on success and false if one of the underlying parser
 * components fail (rare).
 */
bool
SBMLWriter::writeSBML (const SBMLDocument* d, std::ostream& stream)
{
  bool result = false;

  try
  {
    stream.exceptions(ios_base::badbit | ios_base::failbit | ios_base::eofbit);
    XMLOutputStream xos(stream, "UTF-8", true);
    d->write(xos);
    stream << endl;

    result = true;
  }
  catch (ios_base::failure&)
  {
    SBMLErrorLog *log = (const_cast<SBMLDocument *>(d))->getErrorLog();
    log->logError(XMLFileOperationError);
  }

  return result;
}


/*
 * Writes the given SBML document to an in-memory string and returns a
 * pointer to it.  The string is owned by the caller and should be freed
 * (with free()) when no longer needed.
 *
 * @return the string on success and 0 if one of the underlying parser
 * components fail (rare).
 */
LIBSBML_EXTERN
char*
SBMLWriter::writeToString (const SBMLDocument* d)
{
  ostringstream stream;
  writeSBML(d, stream);

  return safe_strdup( stream.str().c_str() );
}




/**
 * Creates a new SBMLWriter and returns a pointer to it.
 */
LIBSBML_EXTERN
SBMLWriter_t *
SBMLWriter_create ()
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
 * Sets the name of this program. i.\ e.\  the one about to write out the
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
 * Sets the version of this program. i.\ e.\ the one about to write out the
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
 * for writing.
 */
LIBSBML_EXTERN
int
SBMLWriter_writeSBML ( SBMLWriter_t         *sw,
                       const SBMLDocument_t *d,
                       const char           *filename )
{
  return static_cast<int>( sw->writeSBML(d, filename) );
}


/**
 * Writes the given SBML document to an in-memory string and returns a
 * pointer to it.  The string is owned by the caller and should be freed
 * (with free()) when no longer needed.
 *
 * @return the string on success and NULL if one of the underlying parser
 * components fail (rare).
 */
LIBSBML_EXTERN
char *
SBMLWriter_writeSBMLToString (SBMLWriter_t *sw, const SBMLDocument_t *d)
{
  return sw->writeToString(d);
}


/**
 * Writes the given SBML document to filename.  This convenience function
 * is functionally equivalent to:
 *
 *   SBMLWriter_writeSBML(SBMLWriter_create(), d, filename);
 *
 * @return non-zero on success and zero if the filename could not be opened
 * for writing.
 */
LIBSBML_EXTERN
int
writeSBML (const SBMLDocument_t *d, const char *filename)
{
  SBMLWriter sw;
  return static_cast<int>( sw.writeSBML(d, filename) );
}


/**
 * Writes the given SBML document to an in-memory string and returns a
 * pointer to it.  The string is owned by the caller and should be freed
 * (with free()) when no longer needed.  This convenience function is
 * functionally equivalent to:
 *
 *   SBMLWriter_writeSBMLToString(SBMLWriter_create(), d);
 *
 * @return the string on success and NULL if one of the underlying parser
 * components fail (rare).
 */
LIBSBML_EXTERN
char *
writeSBMLToString (const SBMLDocument_t *d)
{
  SBMLWriter sw;
  return sw.writeToString(d);
}
