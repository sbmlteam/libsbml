/**
 * Filename    : SBMLFormatter.hpp
 * Description : Formats SBML ...
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2003-03-07
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


#ifndef SBMLFormatter_hh
#define SBMLFormatter_hh

#include <xercesc/framework/XMLFormatter.hpp>
#include <xercesc/framework/MemBufFormatTarget.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLUniDefs.hpp>

#include "sbml/SBMLTypes.h"
#include "sbml/SBMLUnicodeConstants.hpp" 


//
// The SBMLFormatter is designed to receive SAX-like callback events
// and reconstruct the XML string that produced those events.  This is
// useful if you want your SAX Handler to leave certain chunks of XML
// untouched.  To do so, simply delegate all SAX events (tags) you are not
// interested in to this formatter.  The corresponding XML will be
// reconstructed and can be retrieved with the getString() method (and its
// length with getLength()).
//
// For instance, in the SBML parser, this formatter is used to store the
// original XML contents of <notes> (which can contain XHTML) and
// <annotations> elements.
//
// A formatter can be reset() (emptied) so that it can be reused to process
// another set of XML tags.  The underlying character encoding is
// configurable and defaults to "LATIN1", which is probably safe for most
// applications.
//
// A note on SAX2 qnames vs. localnames:
//
// They are identical if no namespace prefix is specified.  For example,
// suppose:
//
//   <prefix:myelement .../>
//
// In this case, localname will be 'myelement' and qname will be
// 'prefix:myelement'.  If prefix were not present, qname would also be
// 'myelement'.  Since the goal is of SBMLFormatter is to reproduce
// the input as close as possible, qname is used.
//
// See Xerces-C SAX2 API for a description of method parameters.
//
class SBMLFormatter
{

public:

  //
  // Ctor
  //
  // Creates a new SBMLFormatter
  //
  // The underlying character encoding is configurable and defaults to
  // "LATIN1", which is probably safe for most applications.
  //
  SBMLFormatter (const char* outEncoding, XMLFormatTarget* target);

  //
  // Dtor
  //
  ~SBMLFormatter ();

  SBMLFormatter& operator<< ( const SBMLDocument_t             *d   );
  SBMLFormatter& operator<< ( const Model_t                    *m   );
  SBMLFormatter& operator<< ( const Unit_t                     *u   );
  SBMLFormatter& operator<< ( const UnitDefinition_t           *ud  );
  SBMLFormatter& operator<< ( const Compartment_t              *c   );
  SBMLFormatter& operator<< ( const Species_t                  *s   );
  SBMLFormatter& operator<< ( const Parameter_t                *p   );
  SBMLFormatter& operator<< ( const AlgebraicRule_t            *ar  );
  SBMLFormatter& operator<< ( const SpeciesConcentrationRule_t *scr );
  SBMLFormatter& operator<< ( const CompartmentVolumeRule_t    *cvr );
  SBMLFormatter& operator<< ( const ParameterRule_t            *pr  );
  SBMLFormatter& operator<< ( const Reaction_t                 *r   );
  SBMLFormatter& operator<< ( const SpeciesReference_t         *sr  );
  SBMLFormatter& operator<< ( const KineticLaw_t               *kl  );


private:

  void attribute ( const XMLCh* name, bool         value );
  void attribute ( const XMLCh* name, int          value );
  void attribute ( const XMLCh* name, unsigned int value );
  void attribute ( const XMLCh* name, double       value );
  void attribute ( const XMLCh* name, const char*  value );
  void attribute ( const XMLCh* name, const XMLCh* value );


  /**
   * Returns true if the string pointed to by s is empty or s is a NULL
   * pointer.
   */
  inline bool SBMLFormatter::isEmpty(const char *s)
  {
    return !(s && *s);
  }

  inline void SBMLFormatter::startElement (const XMLCh* name)
  {
    *fFormatter << XMLFormatter::NoEscapes << chOpenAngle << name;
  }

  inline void SBMLFormatter::closeStartElement ()
  {
    *fFormatter << XMLFormatter::NoEscapes << chCloseAngle << chLF;
  }

  inline void SBMLFormatter::slashCloseStartElement ()
  {
    *fFormatter << XMLFormatter::NoEscapes
                << chForwardSlash << chCloseAngle << chLF;
  }

  inline void SBMLFormatter::endElement (const XMLCh* name)
  {
    *fFormatter << XMLFormatter::NoEscapes
                << chOpenAngle  << chForwardSlash << name
                << chCloseAngle << chLF;
  }

  inline void SBMLFormatter::upIndent ()   { fIndentLevel++; }
  inline void SBMLFormatter::downIndent () { fIndentLevel--; }


  static const unsigned int NUMBER_BUFFER_SIZE = 100;
  char fNumberBuffer[ NUMBER_BUFFER_SIZE ];

  XMLFormatter*     fFormatter;
  XMLFormatTarget*  fTarget;
  unsigned int      fIndentLevel;
};


#endif  // SBMLFormatter_hh
