/**
 * Filename    : SBMLTagCodes.hpp
 * Description : Maps SBML elements to numbers for comparing and storing
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-12-03
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef SBMLTagCode_hpp
#define SBMLTagCode_hpp


#include "common.hpp"
#include <xercesc/util/XMLUniDefs.hpp>


typedef int SBMLTagCode_t;

static const SBMLTagCode_t TAG_ALGEBRAIC_RULE             =  0;
static const SBMLTagCode_t TAG_ANNOTATION                 =  1;
static const SBMLTagCode_t TAG_ANNOTATIONS                =  2;
static const SBMLTagCode_t TAG_COMPARTMENT                =  3;
static const SBMLTagCode_t TAG_COMPARTMENT_VOLUME_RULE    =  4;
static const SBMLTagCode_t TAG_KINETIC_LAW                =  5;
static const SBMLTagCode_t TAG_LIST_OF_COMPARTMENTS       =  6;
static const SBMLTagCode_t TAG_LIST_OF_PARAMETERS         =  7;
static const SBMLTagCode_t TAG_LIST_OF_PRODUCTS           =  8;
static const SBMLTagCode_t TAG_LIST_OF_REACTANTS          =  9;
static const SBMLTagCode_t TAG_LIST_OF_REACTIONS          = 10;
static const SBMLTagCode_t TAG_LIST_OF_RULES              = 11;
static const SBMLTagCode_t TAG_LIST_OF_SPECIES            = 12;
static const SBMLTagCode_t TAG_LIST_OF_UNIT_DEFINITIONS   = 13;
static const SBMLTagCode_t TAG_LIST_OF_UNITS              = 14;
static const SBMLTagCode_t TAG_MODEL                      = 15;
static const SBMLTagCode_t TAG_NOTES                      = 16;
static const SBMLTagCode_t TAG_PARAMETER                  = 17;
static const SBMLTagCode_t TAG_PARAMETER_RULE             = 18;
static const SBMLTagCode_t TAG_REACTION                   = 19;
static const SBMLTagCode_t TAG_SBML                       = 20;
static const SBMLTagCode_t TAG_SPECIE                     = 21;
static const SBMLTagCode_t TAG_SPECIE_CONCENTRATION_RULE  = 22;
static const SBMLTagCode_t TAG_SPECIE_REFERENCE           = 23;
static const SBMLTagCode_t TAG_SPECIES                    = 24;
static const SBMLTagCode_t TAG_SPECIES_CONCENTRATION_RULE = 25;
static const SBMLTagCode_t TAG_SPECIES_REFERENCE          = 26;
static const SBMLTagCode_t TAG_UNIT                       = 27;
static const SBMLTagCode_t TAG_UNIT_DEFINITION            = 28;
static const SBMLTagCode_t TAG_UNKNOWN                    = 29;


/**
 * Returns the tag code for the given SBML element.
 */
SBMLTagCode_t
SBMLTagCode_forElement (const XMLCh* name);


#endif  // SBMLTagCode_hpp
