/**
 * Filename    : SBMLHashCodeConstants.cpp
 * Description : XML Element hash codes for (fast) numeric comparison
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


#ifndef SBMLHashCodes_hpp
#define SBMLHashCodes_hpp


#include "common.hpp"


typedef int HashCode_t;


/**
 * The following codes were derived from:
 *
 *   HASH_XXX = XMLString::hash(ELEM_XXX, HASH_MODULUS);
 *
 * WHERE ELEM_XXX is the name of the corresponding element, defined in
 * SBMLUnicodeConstants.hpp.
 */
/*
static const unsigned int SBML_HASH_MODULUS = 227;

static const HashCode_t HASH_ALGEBRAIC_RULE             = 172;
static const HashCode_t HASH_ANNOTATION                 =  19;
static const HashCode_t HASH_COMPARTMENT                = 145;
static const HashCode_t HASH_COMPARTMENT_VOLUME_RULE    = 191;
static const HashCode_t HASH_KINETIC_LAW                = 100;
static const HashCode_t HASH_LIST_OF_COMPARTMENTS       = 178;
static const HashCode_t HASH_LIST_OF_PARAMETERS         =  94;
static const HashCode_t HASH_LIST_OF_PRODUCTS           = 185;
static const HashCode_t HASH_LIST_OF_REACTANTS          = 202;
static const HashCode_t HASH_LIST_OF_REACTIONS          =  14;
static const HashCode_t HASH_LIST_OF_RULES              =  13;
static const HashCode_t HASH_LIST_OF_SPECIES            = 183;
static const HashCode_t HASH_LIST_OF_UNIT_DEFINITIONS   =  12;
static const HashCode_t HASH_LIST_OF_UNITS              =  59;
static const HashCode_t HASH_MODEL                      = 203;
static const HashCode_t HASH_NOTES                      =  74;
static const HashCode_t HASH_PARAMETER                  =  58;
static const HashCode_t HASH_PARAMETER_RULE             = 107;
static const HashCode_t HASH_REACTION                   =  99;
static const HashCode_t HASH_SBML                       = 162;
static const HashCode_t HASH_SPECIE                     = 155;
static const HashCode_t HASH_SPECIES                    =   0;
static const HashCode_t HASH_SPECIE_CONCENTRATION_RULE  = 184;
static const HashCode_t HASH_SPECIES_CONCENTRATION_RULE = 149;
static const HashCode_t HASH_SPECIE_REFERENCE           =  22;
static const HashCode_t HASH_SPECIES_REFERENCE          = 180;
static const HashCode_t HASH_UNIT                       = 197;
static const HashCode_t HASH_UNIT_DEFINITION            =  96;
*/

#include <xercesc/util/XMLUniDefs.hpp>


static const HashCode_t HASH_ALGEBRAIC_RULE             =  0;
static const HashCode_t HASH_ANNOTATION                 =  1;
static const HashCode_t HASH_ANNOTATIONS                =  2;
static const HashCode_t HASH_COMPARTMENT                =  3;
static const HashCode_t HASH_COMPARTMENT_VOLUME_RULE    =  4;
static const HashCode_t HASH_KINETIC_LAW                =  5;
static const HashCode_t HASH_LIST_OF_COMPARTMENTS       =  6;
static const HashCode_t HASH_LIST_OF_PARAMETERS         =  7;
static const HashCode_t HASH_LIST_OF_PRODUCTS           =  8;
static const HashCode_t HASH_LIST_OF_REACTANTS          =  9;
static const HashCode_t HASH_LIST_OF_REACTIONS          = 10;
static const HashCode_t HASH_LIST_OF_RULES              = 11;
static const HashCode_t HASH_LIST_OF_SPECIES            = 12;
static const HashCode_t HASH_LIST_OF_UNIT_DEFINITIONS   = 13;
static const HashCode_t HASH_LIST_OF_UNITS              = 14;
static const HashCode_t HASH_MODEL                      = 15;
static const HashCode_t HASH_NOTES                      = 16;
static const HashCode_t HASH_PARAMETER                  = 17;
static const HashCode_t HASH_PARAMETER_RULE             = 18;
static const HashCode_t HASH_REACTION                   = 19;
static const HashCode_t HASH_SBML                       = 20;
static const HashCode_t HASH_SPECIE                     = 21;
static const HashCode_t HASH_SPECIE_CONCENTRATION_RULE  = 22;
static const HashCode_t HASH_SPECIE_REFERENCE           = 23;
static const HashCode_t HASH_SPECIES                    = 24;
static const HashCode_t HASH_SPECIES_CONCENTRATION_RULE = 25;
static const HashCode_t HASH_SPECIES_REFERENCE          = 26;
static const HashCode_t HASH_UNIT                       = 27;
static const HashCode_t HASH_UNIT_DEFINITION            = 28;
static const HashCode_t HASH_UNKNOWN                    = 29;

/**
 * Returns the HashCode for the given SBML element.
 */
HashCode_t
HashCode_forElement(const XMLCh* name);

#endif  // SBMLHashCodes_hpp
