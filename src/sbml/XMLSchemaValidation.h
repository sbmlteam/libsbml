/**
 * Filename    : XMLSchemaValidationLevel.h
 * Description : XML Schema Validation Level
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2004-04-29
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2004 California Institute of Technology and
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


#ifndef XMLSchemValidationLevel_h
#define XMLSchemValidationLevel_h


/**
 * Used by an SBMLReader to indicate the level of XML Schema validation.
 * Schema violations (for both BASIC and FULL) are reported in the
 * SBMLDocument's list of ParseMessages.  The levels are:
 *
 *   NONE (0) turns schema validation off.
 *
 *   BASIC (1) validates an XML instance document against an XML Schema.
 *   Those who wish to perform schema checking on SBML documents should use
 *   this option.
 *
 *   FULL (2) validates both the instance document itself *and* the XML
 *   Schema document.  The XML Schema document is checked for violation of
 *   particle unique attribution constraints and particle derivation
 *   restrictions, which is both time-consuming and memory intensive.
 */
typedef enum
{
    XML_SCHEMA_VALIDATION_NONE
  , XML_SCHEMA_VALIDATION_BASIC
  , XML_SCHEMA_VALIDATION_FULL
} XMLSchemaValidation_t;


#endif  /* XMLSchemValidationLevel_h */
