/**
 * Filename    : XMLUnicodeConstants.hpp
 * Description : Common XML strings in (Xerces-C) Unicode.
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-07-22
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
 *   Stefan Hoops
 */


#ifndef XMLUnicodeConstants_hpp
#define XMLUnicodeConstants_hpp


#ifdef USE_EXPAT
#  include "ExpatUnicodeChars.hpp"
#else
#  include <xercesc/util/XMLUniDefs.hpp>
#endif  // USE_EXPAT


#include "common.hpp"


//
// <?xml version="1.0" encoding="
//
static const XMLCh XML_DECL_1[] =
{
  chOpenAngle, chQuestion,

  chLatin_x, chLatin_m, chLatin_l, chSpace,

  chLatin_v, chLatin_e, chLatin_r, chLatin_s, chLatin_i, chLatin_o,
  chLatin_n,

  chEqual,

  chDoubleQuote, chDigit_1, chPeriod, chDigit_0, chDoubleQuote, chSpace,

  chLatin_e, chLatin_n, chLatin_c, chLatin_o, chLatin_d, chLatin_i,
  chLatin_n, chLatin_g,

  chEqual, chDoubleQuote, chNull
};


//
// "?>
//
static const XMLCh XML_DECL_2[] =
{
  chDoubleQuote, chQuestion, chCloseAngle, chLF, chNull
};


//
// 1.0
//
static const XMLCh XML_VERSION_1_0[] =
{
  chDigit_1, chPeriod, chDigit_0, chNull
};


//
// 1.1
//
static const XMLCh XML_VERSION_1_1[] =
{
  chDigit_1, chPeriod, chDigit_1, chNull
};


#endif  // XMLUnicodeConstants_hpp
