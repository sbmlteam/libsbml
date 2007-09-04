/**
 * @file    SBMLReader.cpp
 * @brief   Reads an SBML Document into memory
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

#include <sbml/xml/XMLError.h>
#include <sbml/xml/XMLErrorLog.h>
#include <sbml/xml/XMLInputStream.h>

#include <sbml/SBMLErrorLog.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLError.h>
#include <sbml/Model.h>
#include <sbml/SBMLReader.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new SBMLReader and returns it. 
 */
SBMLReader::SBMLReader ()
{
}


/**
 * Destorys this SBMLReader.
 */
SBMLReader::~SBMLReader ()
{
}


/**
 * Reads an SBML document from the given file.  If filename does not exist
 * or is not an SBML file, an error will be logged.  Errors can be
 * identified by their unique ids, e.g.:
 *
 * <code>
 *   SBMLDocument* d = reader.readSBML(filename);
 *
 *   if (d->getNumErrors() > 0)\n
 *   {\n
 *     if (d->getError(0)->getId() == SBML_READ_ERROR_FILE_NOT_FOUND)\n
 *     if (d->getError(0)->getId() == SBML_READ_ERROR_NOT_SBML)\n
 *   }\n
 * </code>
 *
 * @return a pointer to the SBMLDocument read.
 */
SBMLDocument*
SBMLReader::readSBML (const std::string& filename)
{
  return readInternal(filename.c_str(), true);
}


/**
 * Reads an SBML document from the given XML string.
 *
 * If the string does not begin with XML declaration:
 *
 *   <?xml version='1.0' encoding='UTF-8'?>
 *
 * it will be prepended.
 *
 * This method will log a fatal error if the XML string is not SBML.  See
 * the method documentation for readSBML(filename) for example error
 * checking code.
 *
 * @return a pointer to the SBMLDocument read.
 */
SBMLDocument*
SBMLReader::readSBMLFromString (const std::string& xml)
{
  return readInternal(xml.c_str(), false);
}


/** @cond doxygen-libsbml-internal */
static bool
isCriticalError(const unsigned int errorId)
{
  switch (errorId)
  {
  case XMLError::InternalParserError:
  case XMLError::UnrecognizedParserCode:
  case XMLError::TranscoderError:
  case XMLError::NotWellFormed:
  case XMLError::InvalidConstruct:
  case XMLError::UnclosedToken:
  case XMLError::TagMismatch:
  case XMLError::BadPrefix:
  case XMLError::MissingAttributeValue:
  case XMLError::BadXMLComment:
  case XMLError::UnexpectedEOF:
  case XMLError::UninterpretableContent:
  case XMLError::BadDocumentStructure:
  case XMLError::InvalidAfterContent:
  case XMLError::ExpectedQuotedString:
  case XMLError::EmptyValueNotPermitted:
  case XMLError::MissingElements:
  case XMLError::BadXMLDeclLocation:
    return true;

  default:
    return false;
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Used by readSBML() and readSBMLFromString().
 */
SBMLDocument*
SBMLReader::readInternal (const char* content, bool isFile)
{
  SBMLDocument* d = new SBMLDocument();

  if (isFile && content && (util_file_exists(content) == false))
  {
    d->getErrorLog()->logError(XMLError::FileUnreadable);
  }
  else
  {
    XMLInputStream stream(content, isFile, "", d->getErrorLog());

    d->read(stream);
    
    if (stream.isError())
    {
      // If we encountered an error, some parsers will report it sooner
      // than others.  Unfortunately, those that fail sooner do it in an
      // opaque call, so we can't change the behavior.  Since we don't want
      // different parsers to report different validation errors, we bring
      // all parsers back to the same point.

      d->setModel(NULL);

      for (unsigned int i = 0; i < d->getNumErrors(); ++i)      
      {
        if (isCriticalError(d->getError(i)->getErrorId()))
        {
          // If we find even one critical error, all other errors are
          // suspect and may be bogus.  Remove them.

          for (int n = d->getNumErrors()-1; n >= 0; n--)      
            if (!isCriticalError(d->getError(n)->getErrorId()))
            {
              d->getErrorLog()->remove(d->getError(n)->getErrorId());
            }

          break;
        }
      }
    }
    else
    {
      // Low-level XML errors will have been caught in the first read,
      // before we even attempt to interpret the content as SBML.  Here
      // we want to start checking some basic SBML-level errors.

      if (stream.getEncoding() == "")
      {
        d->getErrorLog()->logError(XMLError::MissingXMLEncoding);
      }
      else if (strcmp_insensitive(stream.getEncoding().c_str(), "UTF-8") != 0)
      {
        d->getErrorLog()->logError(SBMLError::NotUTF8);
      }

      if (d->getModel() == 0)
      {
        d->getErrorLog()->logError(SBMLError::MissingModel);
      }
      else if (d->getLevel() == 1)
      {
	// In Level 1, some listOfElements were required.

        if (d->getModel()->getNumCompartments() == 0)
        {
          d->getErrorLog()->logError(SBMLError::NotSchemaConformant,
				     d->getLevel(), d->getVersion(), 
            "An SBML Level 1 model must contain at least one <compartment>.");
        }

        if (d->getVersion() == 1)
        {
          if (d->getModel()->getNumSpecies() == 0)
          {
            d->getErrorLog()->logError(SBMLError::NotSchemaConformant,
				       d->getLevel(), d->getVersion(), 
            "An SBML Level 1 Version 1 model must contain at least one <species>.");
          }
          if (d->getModel()->getNumReactions() == 0)
          {
            d->getErrorLog()->logError(SBMLError::NotSchemaConformant,
				       d->getLevel(), d->getVersion(), 
            "An SBML Level 1 Version 1 model must contain at least one <reaction>.");
          }
        }
      }
    }
  }
  return d;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */


/**
 * Creates a new SBMLReader and returns it. 
 */
LIBSBML_EXTERN
SBMLReader_t *
SBMLReader_create ()
{
  return new (nothrow) SBMLReader;
}


/**
 * Frees the given SBMLReader.
 */
LIBSBML_EXTERN
void
SBMLReader_free (SBMLReader_t *sr)
{
  delete sr;
}


/**
 * Reads an SBML document from the given file.  If filename does not exist
 * or is not an SBML file, an error will be logged.  Errors can be
 * identified by their unique ids, e.g.:
 *
 * <code>
 *   SBMLReader_t   *sr;\n
 *   SBMLDocument_t *d;
 *
 *   sr = SBMLReader_create();
 *
 *   d = SBMLReader_readSBML(reader, filename);
 *
 *   if (SBMLDocument_getNumErrors(d) > 0)\n
 *   {\n
 *     if (XMLError_getId(SBMLDocument_getError(d, 0))
 *                                           == SBML_READ_ERROR_FILE_NOT_FOUND)\n
 *     if (XMLError_getId(SBMLDocument_getError(d, 0))
 *                                           == SBML_READ_ERROR_NOT_SBML)\n
 *   }\n
 * </code>
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBML (SBMLReader_t *sr, const char *filename)
{
  return (filename != NULL) ? sr->readSBML(filename) : sr->readSBML("");
}


/**
 * Reads an SBML document from the given XML string.
 *
 * If the string does not begin with XML declaration:
 *
 *   <?xml version='1.0' encoding='UTF-8'?>
 *
 * it will be prepended.
 *
 * This method will log a fatal error if the XML string is not SBML.  See
 * the method documentation for readSBML(filename) for example error
 * checking code.
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBMLFromString (SBMLReader_t *sr, const char *xml)
{
  return (xml != NULL) ? sr->readSBMLFromString(xml) :
                         sr->readSBMLFromString("");
}


/**
 * Reads an SBML document from the given file.  If filename does not exist
 * or is not an SBML file, an error will be logged.  Errors can be
 * identified by their unique ids, e.g.:
 *
 * <code>
 *   SBMLReader_t   *sr;\n
 *   SBMLDocument_t *d;
 *
 *   sr = SBMLReader_create();
 *
 *   d = SBMLReader_readSBML(reader, filename);
 *
 *   if (SBMLDocument_getNumErrors(d) > 0)\n
 *   {\n
 *     if (XMLError_getId(SBMLDocument_getError(d, 0))
 *                                           == SBML_READ_ERROR_FILE_NOT_FOUND)\n
 *     if (XMLError_getId(SBMLDocument_getError(d, 0))
 *                                           == SBML_READ_ERROR_NOT_SBML)\n
 *   }\n
 * </code>
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBML (const char *filename)
{
  SBMLReader sr;
  return sr.readSBML(filename);
}


/**
 * Reads an SBML document from the given XML string.
 *
 * If the string does not begin with XML declaration:
 *
 *   <?xml version='1.0' encoding='UTF-8'?>
 *
 * it will be prepended.
 *
 * This method will log a fatal error if the XML string is not SBML.  See
 * the method documentation for readSBML(filename) for example error
 * checking code.
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBMLFromString (const char *xml)
{
  SBMLReader sr;
  return sr.readSBMLFromString(xml);
}


/** @endcond doxygen-c-only */
