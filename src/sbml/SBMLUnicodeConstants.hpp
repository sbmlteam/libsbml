/**
 * Filename    : SBMLUnicodeConstants.hpp
 * Description : SBML Attribute and Element names in the (Xerces-C) Unicode.
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-11-08
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


#ifndef SBMLUnicodeConstants_hpp
#define SBMLUnicodeConstants_hpp


#include <xercesc/util/XMLUniDefs.hpp>
#include "common.hpp"


//
// http://www.sbml.org/sbml/level1
//
static const XMLCh XMLNS_SBML_L1[] =
{
  chLatin_h, chLatin_t, chLatin_t, chLatin_p,
  chColon, chForwardSlash, chForwardSlash,
  chLatin_w, chLatin_w, chLatin_w,
  chPeriod,
  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chPeriod,
  chLatin_o, chLatin_r, chLatin_g,
  chForwardSlash,
  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chForwardSlash,
  chLatin_l, chLatin_e, chLatin_v, chLatin_e, chLatin_l, chDigit_1,
  chNull
};


static const XMLCh ELEM_ALGEBRAIC_RULE[] =
{
  chLatin_a, chLatin_l, chLatin_g, chLatin_e, chLatin_b, chLatin_r,
  chLatin_a, chLatin_i, chLatin_c, chLatin_R, chLatin_u, chLatin_l,
  chLatin_e, chNull
};

static const XMLCh ELEM_ANNOTATION[] =
{
  chLatin_a, chLatin_n, chLatin_n, chLatin_o, chLatin_t, chLatin_a,
  chLatin_t, chLatin_i, chLatin_o, chLatin_n, chNull
};

static const XMLCh ELEM_ANNOTATIONS[] =
{
  chLatin_a, chLatin_n, chLatin_n, chLatin_o, chLatin_t, chLatin_a,
  chLatin_t, chLatin_i, chLatin_o, chLatin_n, chLatin_s, chNull
};

static const XMLCh ELEM_COMPARTMENT[] =
{
  chLatin_c, chLatin_o, chLatin_m, chLatin_p, chLatin_a, chLatin_r,
  chLatin_t, chLatin_m, chLatin_e, chLatin_n, chLatin_t, chNull
};

static const XMLCh ELEM_COMPARTMENT_VOLUME_RULE[] =
{
  chLatin_c, chLatin_o, chLatin_m, chLatin_p, chLatin_a, chLatin_r,
  chLatin_t, chLatin_m, chLatin_e, chLatin_n, chLatin_t, chLatin_V,
  chLatin_o, chLatin_l, chLatin_u, chLatin_m, chLatin_e, chLatin_R,
  chLatin_u, chLatin_l, chLatin_e, chNull
};

static const XMLCh ELEM_KINETIC_LAW[] =
{
  chLatin_k, chLatin_i, chLatin_n, chLatin_e, chLatin_t, chLatin_i,
  chLatin_c, chLatin_L, chLatin_a, chLatin_w, chNull
};

static const XMLCh ELEM_LIST_OF_COMPARTMENTS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f,
  chLatin_C, chLatin_o, chLatin_m, chLatin_p, chLatin_a, chLatin_r,
  chLatin_t, chLatin_m, chLatin_e, chLatin_n, chLatin_t, chLatin_s,
  chNull
};

static const XMLCh ELEM_LIST_OF_PARAMETERS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f,
  chLatin_P, chLatin_a, chLatin_r, chLatin_a, chLatin_m, chLatin_e,
  chLatin_t, chLatin_e, chLatin_r, chLatin_s, chNull
};

static const XMLCh ELEM_LIST_OF_PRODUCTS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f,
  chLatin_P, chLatin_r, chLatin_o, chLatin_d, chLatin_u, chLatin_c,
  chLatin_t, chLatin_s, chNull
};

static const XMLCh ELEM_LIST_OF_REACTANTS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f,
  chLatin_R, chLatin_e, chLatin_a, chLatin_c, chLatin_t, chLatin_a,
  chLatin_n, chLatin_t, chLatin_s, chNull
};

static const XMLCh ELEM_LIST_OF_REACTIONS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f,
  chLatin_R, chLatin_e, chLatin_a, chLatin_c, chLatin_t, chLatin_i,
  chLatin_o, chLatin_n, chLatin_s, chNull
};

static const XMLCh ELEM_LIST_OF_RULES[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f,
  chLatin_R, chLatin_u, chLatin_l, chLatin_e, chLatin_s, chNull
};

static const XMLCh ELEM_LIST_OF_SPECIES[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f,
  chLatin_S, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e,
  chLatin_s, chNull
};

static const XMLCh ELEM_LIST_OF_UNIT_DEFINITIONS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f,
  chLatin_U, chLatin_n, chLatin_i, chLatin_t, chLatin_D, chLatin_e,
  chLatin_f, chLatin_i, chLatin_n, chLatin_i, chLatin_t, chLatin_i,
  chLatin_o, chLatin_n, chLatin_s, chNull
};

static const XMLCh ELEM_LIST_OF_UNITS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f,
  chLatin_U, chLatin_n, chLatin_i, chLatin_t, chLatin_s, chNull
};


static const XMLCh ELEM_MODEL[] =
{
  chLatin_m, chLatin_o, chLatin_d, chLatin_e, chLatin_l, chNull
};

static const XMLCh ELEM_NOTES[] =
{
  chLatin_n, chLatin_o, chLatin_t, chLatin_e, chLatin_s, chNull
};

static const XMLCh ELEM_PARAMETER[] =
{
  chLatin_p, chLatin_a, chLatin_r, chLatin_a, chLatin_m, chLatin_e,
  chLatin_t, chLatin_e, chLatin_r, chNull
};

static const XMLCh ELEM_PARAMETER_RULE[] =
{
  chLatin_p, chLatin_a, chLatin_r, chLatin_a, chLatin_m, chLatin_e,
  chLatin_t, chLatin_e, chLatin_r, chLatin_R, chLatin_u, chLatin_l,
  chLatin_e, chNull
};

static const XMLCh ELEM_REACTION[] =
{
  chLatin_r, chLatin_e, chLatin_a, chLatin_c, chLatin_t, chLatin_i,
  chLatin_o, chLatin_n, chNull
};

static const XMLCh ELEM_SBML[] =
{
  chLatin_s, chLatin_b, chLatin_m, chLatin_l, chNull
};

static const XMLCh ELEM_SPECIE[] =
{
  chLatin_s, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e, chNull
};

static const XMLCh ELEM_SPECIES[] =
{
  chLatin_s, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e,
  chLatin_s, chNull
};

static const XMLCh ELEM_SPECIE_CONCENTRATION_RULE[] =
{
  chLatin_s, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e,
  chLatin_C, chLatin_o, chLatin_n, chLatin_c, chLatin_e, chLatin_n,
  chLatin_t, chLatin_r, chLatin_a, chLatin_t, chLatin_i, chLatin_o,
  chLatin_n, chLatin_R, chLatin_u, chLatin_l, chLatin_e, chNull
};

static const XMLCh ELEM_SPECIES_CONCENTRATION_RULE[] =
{
  chLatin_s, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e,
  chLatin_s, chLatin_C, chLatin_o, chLatin_n, chLatin_c, chLatin_e,
  chLatin_n, chLatin_t, chLatin_r, chLatin_a, chLatin_t, chLatin_i,
  chLatin_o, chLatin_n, chLatin_R, chLatin_u, chLatin_l, chLatin_e,
  chNull
};

static const XMLCh ELEM_SPECIE_REFERENCE[] =
{
  chLatin_s, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e,
  chLatin_R, chLatin_e, chLatin_f, chLatin_e, chLatin_r, chLatin_e,
  chLatin_n, chLatin_c, chLatin_e, chNull
};

static const XMLCh ELEM_SPECIES_REFERENCE[] =
{
  chLatin_s, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e,
  chLatin_s, chLatin_R, chLatin_e, chLatin_f, chLatin_e, chLatin_r,
  chLatin_e, chLatin_n, chLatin_c, chLatin_e, chNull
};

static const XMLCh ELEM_UNIT[] =
{
  chLatin_u, chLatin_n, chLatin_i, chLatin_t, chNull
};

static const XMLCh ELEM_UNIT_DEFINITION[] =
{
  chLatin_u, chLatin_n, chLatin_i, chLatin_t, chLatin_D, chLatin_e,
  chLatin_f, chLatin_i, chLatin_n, chLatin_i, chLatin_t, chLatin_i,
  chLatin_o, chLatin_n, chNull
};

static const XMLCh ATTR_BOUNDARY_CONDITION[] =
{
  chLatin_b, chLatin_o, chLatin_u, chLatin_n, chLatin_d, chLatin_a,
  chLatin_r, chLatin_y, chLatin_C, chLatin_o, chLatin_n, chLatin_d,
  chLatin_i, chLatin_t, chLatin_i, chLatin_o, chLatin_n, chNull
};

static const XMLCh ATTR_CHARGE[] =
{
  chLatin_c, chLatin_h, chLatin_a, chLatin_r, chLatin_g, chLatin_e,
  chNull
};

static const XMLCh ATTR_COMPARTMENT[] =
{
  chLatin_c, chLatin_o, chLatin_m, chLatin_p, chLatin_a, chLatin_r,
  chLatin_t, chLatin_m, chLatin_e, chLatin_n, chLatin_t, chNull
};

static const XMLCh ATTR_DENOMINATOR[] =
{
  chLatin_d, chLatin_e, chLatin_n, chLatin_o, chLatin_m, chLatin_i,
  chLatin_n, chLatin_a, chLatin_t, chLatin_o, chLatin_r, chNull
};

static const XMLCh ATTR_EXPONENT[] =
{
  chLatin_e, chLatin_x, chLatin_p, chLatin_o, chLatin_n, chLatin_e,
  chLatin_n, chLatin_t, chNull
};

static const XMLCh ATTR_FAST[] =
{
  chLatin_f, chLatin_a, chLatin_s, chLatin_t, chNull
};

static const XMLCh ATTR_FORMULA[] =
{
  chLatin_f, chLatin_o, chLatin_r, chLatin_m, chLatin_u, chLatin_l,
  chLatin_a, chNull
};

static const XMLCh ATTR_INITIAL_AMOUNT[] =
{
  chLatin_i, chLatin_n, chLatin_i, chLatin_t, chLatin_i, chLatin_a,
  chLatin_l, chLatin_A, chLatin_m, chLatin_o, chLatin_u, chLatin_n,
  chLatin_t, chNull
};

static const XMLCh ATTR_KIND[] =
{
  chLatin_k, chLatin_i, chLatin_n, chLatin_d, chNull
};

static const XMLCh ATTR_LEVEL[] =
{
  chLatin_l, chLatin_e, chLatin_v, chLatin_e, chLatin_l, chNull
};

static const XMLCh ATTR_NAME[] =
{
  chLatin_n, chLatin_a, chLatin_m, chLatin_e, chNull
};

static const XMLCh ATTR_OUTSIDE[] =
{
  chLatin_o, chLatin_u, chLatin_t, chLatin_s, chLatin_i, chLatin_d,
  chLatin_e, chNull
};

static const XMLCh ATTR_REVERSIBLE[] =
{
  chLatin_r, chLatin_e, chLatin_v, chLatin_e, chLatin_r, chLatin_s,
  chLatin_i, chLatin_b, chLatin_l, chLatin_e, chNull
};

static const XMLCh ATTR_SCALE[] =
{
  chLatin_s, chLatin_c, chLatin_a, chLatin_l, chLatin_e, chNull
};

static const XMLCh ATTR_SPECIE[] =
{
  chLatin_s, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e, chNull
};

static const XMLCh ATTR_SPECIES[] =
{
  chLatin_s, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e,
  chLatin_s, chNull
};

static const XMLCh ATTR_STOICHIOMETRY[] =
{
  chLatin_s, chLatin_t, chLatin_o, chLatin_i, chLatin_c, chLatin_h,
  chLatin_i, chLatin_o, chLatin_m, chLatin_e, chLatin_t, chLatin_r,
  chLatin_y, chNull
};

static const XMLCh ATTR_SUBSTANCE_UNITS[] =
{
  chLatin_s, chLatin_u, chLatin_b, chLatin_s, chLatin_t, chLatin_a,
  chLatin_n, chLatin_c, chLatin_e, chLatin_U, chLatin_n, chLatin_i,
  chLatin_t, chLatin_s, chNull
};

static const XMLCh ATTR_TIME_UNITS[] =
{
  chLatin_t, chLatin_i, chLatin_m, chLatin_e, chLatin_U, chLatin_n,
  chLatin_i, chLatin_t, chLatin_s, chNull
};

static const XMLCh ATTR_TYPE[] =
{
  chLatin_t, chLatin_y, chLatin_p, chLatin_e, chNull
};

static const XMLCh ATTR_UNITS[] =
{
  chLatin_u, chLatin_n, chLatin_i, chLatin_t, chLatin_s, chNull
};

static const XMLCh ATTR_VERSION[] =
{
  chLatin_v, chLatin_e, chLatin_r, chLatin_s, chLatin_i, chLatin_o,
  chLatin_n, chNull
};

static const XMLCh ATTR_VALUE[] =
{
  chLatin_v, chLatin_a, chLatin_l, chLatin_u, chLatin_e, chNull
};

static const XMLCh ATTR_VOLUME[] =
{
  chLatin_v, chLatin_o, chLatin_l, chLatin_u, chLatin_m, chLatin_e,
  chNull
};

static const XMLCh ATTR_XMLNS[] =
{
  chLatin_x, chLatin_m, chLatin_l, chLatin_n, chLatin_s, chNull
};

static const XMLCh VAL_TRUE[] =
{
  chLatin_t, chLatin_r, chLatin_u, chLatin_e, chNull
};

static const XMLCh VAL_FALSE[] =
{
  chLatin_f, chLatin_a, chLatin_l, chLatin_s, chLatin_e, chNull
};

static const XMLCh VAL_NAN[] =
{
  chLatin_N, chLatin_a, chLatin_N, chNull
};

static const XMLCh VAL_INF[] =
{
  chLatin_I, chLatin_N, chLatin_F, chNull
};

static const XMLCh VAL_NEG_INF[] =
{
  chDash, chLatin_I, chLatin_N, chLatin_F, chNull
};

static const XMLCh VAL_NEG_ZERO[] =
{
  chDash, chDigit_0, chNull
};

static const XMLCh VAL_0[] =
{
  chDigit_0, chNull
};

static const XMLCh VAL_1[] =
{
  chDigit_1, chNull
};


#endif  // SBMLUnicodeConstants_hpp
