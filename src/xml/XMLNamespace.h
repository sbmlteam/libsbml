/**
 * \file    XMLNamespace.h
 * \brief   An XMLNamespace is a (prefix, URI) pair
 * \author  Ben Bornstein
 * 
 * $Id$
 * $Source$
 */
/* Copyright 2004 California Institute of Technology and
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


#ifndef XMLNamespace_h
#define XMLNamespace_h


#ifdef __cplusplus


#include <string>
#include "../common/extern.h"


/**
 * An XMLNamespace is a (prefix, URI) pair.  XML namespaces definitions are
 * written as:
 *
 *   xmlns:prefix="URI"
 *
 * and later the prefix may be used to put either elements and/or
 * attributes in that namespace:
 *
 *   <prefix:element prefix:attribute1="..."  ... >
 */
class XMLNamespace
{
public:

  /**
   * Creates a new XMLNamespace with prefix and URI.
   *
   * If prefix starts with 'xmlns:' (case-insensitive), it will be removed.
   */
  LIBSBML_EXTERN
  XMLNamespace (const std::string& prefix, const std::string& URI);

  /**
   * @return the prefix for this XMLNamespace.
   */
  LIBSBML_EXTERN
  const std::string& getPrefix () const  { return mPrefix; }

  /**
   * @return the URI for this XMLNamespace.
   */
  LIBSBML_EXTERN
  const std::string& getURI () const     { return mURI; }

  /**
   * @return true if prefix begins with 'xmlns:' (case-insensitive), false
   * otherwise.
   */
  LIBSBML_EXTERN
  static bool startsWithXMLNS (const std::string& prefix);


protected:

  std::string mPrefix;
  std::string mURI;
};


#endif  /* _cplusplus */
#endif  /* XMLNamespace_h */
