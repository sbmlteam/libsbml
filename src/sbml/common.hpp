/**
 * Filename    : common.hpp
 * Description : Common functions and macros for C++ only
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-03-13
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


#ifndef SBML_COMMON_HPP
#define SBML_COMMON_HPP 1


#include <xercesc/util/XercesVersion.hpp>
#include <xercesc/util/XercesDefs.hpp>


//
// Starting with Xerces-C++ 2.2.0 its API is wrapped in a C++ namesapce.
//
// The namespaces is version specific, e.g. xercesc_2_2, but the macro
// XERCES_CPP_NAMESPACE_USE defines "using namespace xercesc<version>;".
// If Xerces was compiled *without* namespace support, this macro will be
// empty.
//
// The namespace xercesc:: is also valid.  It is an alias for the most
// current version of API.
//
// _XERCES_VERSION (20200 == 2.02.00 == 2.2.0) is defined in
// XercesVersion.hpp.  XERCES_CPP_NAMESPACE_USE is defined in
// XercesDefs.hpp.
//
#if _XERCES_VERSION >= 20200
XERCES_CPP_NAMESPACE_USE
#endif


#endif  // SBML_COMMON_HPP
