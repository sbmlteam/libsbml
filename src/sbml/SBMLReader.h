/**
 * Filename    : SBMLReader.h
 * Description : Reads an SBML Document into memory
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-10-15
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


#ifndef SBMLReader_h
#define SBMLReader_h


#include "extern.h"
#include "SBMLDocument.h"
#include "XMLSchemaValidation.h"


BEGIN_C_DECLS


/**
 * An SBMLReader maintains some simple state related to XML Schema
 * validation.
 */
typedef void SBMLReader_t;


/**
 * Creates a new SBMLReader and returns a pointer to it.
 *
 * By default schema validation is off (XML_SCHEMA_VALIDATION_NONE) and
 * schemaFilename is NULL.
 */
LIBSBML_EXTERN
SBMLReader_t *
SBMLReader_create (void);

/**
 * Frees the given SBMLReader.
 */
LIBSBML_EXTERN
void
SBMLReader_free (SBMLReader_t *sr);


/**
 * @return the schema filename used by this SBMLReader to validate SBML
 * Level 1 version 1 documents.
 */
LIBSBML_EXTERN
const char *
SBMLReader_getSchemaFilenameL1v1 (const SBMLReader_t *sr);

/**
 * @return the schema filename used by this SBMLReader to validate SBML
 * Level 1 version 2 documents.
 */
LIBSBML_EXTERN
const char *
SBMLReader_getSchemaFilenameL1v2 (const SBMLReader_t *sr);

/**
 * @return the schema filename used by this SBMLReader to validate SBML Level
 * 2 version 1 documents.
 */
LIBSBML_EXTERN
const char *
SBMLReader_getSchemaFilenameL2v1 (const SBMLReader_t *sr);

/**
 * @return the schema validation level used by this SBMLReader.
 */
LIBSBML_EXTERN
XMLSchemaValidation_t
SBMLReader_getSchemaValidationLevel(const SBMLReader_t *sr);

/**
 * Reads the SBML document from the given file and returns a pointer to it.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBML (SBMLReader_t *sr, const char *filename);

/**
 * Reads the SBML document from the given XML string and returns a pointer
 * to it.
 *
 * The XML string must be complete and legal XML document.  Among other
 * things, it must start with an XML processing instruction.  For e.g.,:
 *
 *   <?xml version='1.0' encoding='UTF-8'?>
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBMLFromString (SBMLReader_t *sr, const char *xml);


/**
 * Reads the SBML document from the given file and returns a pointer to it.
 * This convenience function is functionally equivalent to:
 *
 *   SBMLReader_readSBML(SBMLReader_create(), filename);
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBML (const char *filename);

/**
 * Reads the SBML document from the given XML string and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   SBMLReader_readSBMLFromString(SBMLReader_create(), filename);
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBMLFromString (const char *xml);


/**
 * Sets the schema filename used by this SBMLReader to validate SBML Level
 * 1 version 1 documents.
 *
 * The filename should be either i) an absolute path or ii) relative to the
 * directory contain the SBML file(s) to be read.
 */
LIBSBML_EXTERN
void
SBMLReader_setSchemaFilenameL1v1 (SBMLReader_t *sr, const char *filename);

/**
 * Sets the schema filename used by this SBMLReader to validate SBML Level
 * 1 version 2 documents.
 *
 * The filename should be either i) an absolute path or ii) relative to the
 * directory contain the SBML file(s) to be read.
 */
LIBSBML_EXTERN
void
SBMLReader_setSchemaFilenameL1v2 (SBMLReader_t *sr, const char *filename);

/**
 * Sets the schema filename used by this SBMLReader to validate SBML Level
 * 2 version 1 documents.
 *
 * The filename should be either i) an absolute path or ii) relative to the
 * directory contain the SBML file(s) to be read.
 */
LIBSBML_EXTERN
void
SBMLReader_setSchemaFilenameL2v1 (SBMLReader_t *sr, const char *filename);


/**
 * Sets the schema validation level used by this SBMLReader.
 *
 * The levels are:
 *
 *   XML_SCHEMA_VALIDATION_NONE (0) turns schema validation off.
 *
 *   XML_SCHEMA_VALIDATION_BASIC (1) validates an XML instance document
 *   against an XML Schema.  Those who wish to perform schema checking on
 *   SBML documents should use this option.
 *
 *   XML_SCHEMA_VALIDATION_FULL (2) validates both the instance document
 *   itself *and* the XML Schema document.  The XML Schema document is
 *   checked for violation of particle unique attribution constraints and
 *   particle derivation restrictions, which is both time-consuming and
 *   memory intensive.
 */
LIBSBML_EXTERN
void
SBMLReader_setSchemaValidationLevel ( SBMLReader_t *sr,
                                      XMLSchemaValidation_t level );


END_C_DECLS


#endif  /** SBMLReader_h **/
