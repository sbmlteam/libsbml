/**
 * Filename    : XMLNamespace.cpp
 * Description : An XMLNamespace is a (prefix, URI) pair
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2004-09-15
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
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/XMLNamespace.hpp"


using namespace std;


/**
 * Creates a new XMLNamespace with prefix and URI.
 *
 * If prefix starts with 'xmlns:' (case-insensitive), it will be removed.
 */
LIBSBML_EXTERN
XMLNamespace::XMLNamespace (const string& prefix, const string& URI) :
  mPrefix(prefix), mURI(URI)
{
  if ( startsWithXMLNS(mPrefix) ) mPrefix.erase(0, 6);
}


/**
 * @return true if prefix begins with 'xmlns:' (case-insensitive), false
 * otherwise.
 */
bool
XMLNamespace::startsWithXMLNS (const string& prefix)
{
  return
    prefix.length() > 6                    &&
    (prefix[0] == 'x' || prefix[0] == 'X') &&
    (prefix[1] == 'm' || prefix[1] == 'M') &&
    (prefix[2] == 'l' || prefix[2] == 'L') &&
    (prefix[3] == 'n' || prefix[3] == 'N') &&
    (prefix[4] == 's' || prefix[4] == 'S') &&
     prefix[5] == ':';
}
