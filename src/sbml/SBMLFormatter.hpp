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

#include "List.h"
#include "SBMLTypes.h"

#include "common.hpp"
#include "SBMLUnicodeConstants.hpp" 


//
// SBMLFormatter is meant to act like a C++ output stream.  Creating an
// SBMLFormatter requires a character encoding and an underlying
// XMLFormatTarget, which can be either in-memory (with MemBufFormatTarget)
// or file (FileFormatTarget), to be specified.  Once created, inserting
// SBML objects (C structs) into the stream (with <<) will cause them to be
// formatted in the character encoding for the XMLFormatTarget.
//
//
// By default, the formatter outputs SBML Level 1, version 2 (L1v2).  To
// change the default, either i) insert an SBMLDocument into the formatter
// stream and the formatter will use the level and version of the given
// SBMLDocument (most common) or ii) insert a level or version directly
// (usually used unit test purposes) as in:
//
//   formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
//
// Currently, this class is meant to be used internally by libsbml.
//
class SBMLFormatter
{

public:

  //
  // Ctor
  //
  // Creates a new SBMLFormatter with the given character encoding.
  //
  SBMLFormatter (const char* outEncoding, XMLFormatTarget* target);

  //
  // Dtor
  //
  ~SBMLFormatter ();


  enum SBMLLevel_t   { Level1   = 1 };
  enum SBMLVersion_t { Version1 = 1, Version2 = 2 };

  /**
   * Sets the SBML Level number (used to format subsequent insertions).
   */
  SBMLFormatter& operator<< (const SBMLLevel_t level);

  /**
   * Sets the SBML Version number (used to format subsequent insertions).
   */
  SBMLFormatter& operator<< (const SBMLVersion_t version);

  //
  // Insertion operators for specific SBML types.
  //

  SBMLFormatter& operator<< ( const SBMLDocument_t*             d   );
  SBMLFormatter& operator<< ( const Model_t*                    m   );
  SBMLFormatter& operator<< ( const UnitDefinition_t*           ud  );
  SBMLFormatter& operator<< ( const Unit_t*                     u   );
  SBMLFormatter& operator<< ( const Compartment_t*              c   );
  SBMLFormatter& operator<< ( const Species_t*                  s   );
  SBMLFormatter& operator<< ( const Parameter_t*                p   );
  SBMLFormatter& operator<< ( const Rule_t*                     r   );
  SBMLFormatter& operator<< ( const AlgebraicRule_t*            ar  );
  SBMLFormatter& operator<< ( const SpeciesConcentrationRule_t* scr );
  SBMLFormatter& operator<< ( const CompartmentVolumeRule_t*    cvr );
  SBMLFormatter& operator<< ( const ParameterRule_t*            pr  );
  SBMLFormatter& operator<< ( const Reaction_t*                 r   );
  SBMLFormatter& operator<< ( const SpeciesReference_t*         sr  );
  SBMLFormatter& operator<< ( const KineticLaw_t*               kl  );


private:

  void listOfUnitDefinitions ( List_t* list );
  void listOfUnits           ( List_t* list );
  void listOfCompartments    ( List_t* list );
  void listOfSpecies         ( List_t* list );
  void listOfParameters      ( List_t* list );
  void listOfRules           ( List_t* list );
  void listOfReactions       ( List_t* list );
  void listOfReactants       ( List_t* list );
  void listOfProducts        ( List_t* list );

  void annotation (const char* s);
  void notes      (const char* s);

  inline void notesAndAnnotation (const SBase_t* sb);

  void ruleType (const RuleType_t type);

  /**
   * Returns true if the string pointed to by s is NULL or zero-length.
   */
  inline bool isEmpty (const char* s);

  //
  // In this context "empty" means either no notes, annotations and other
  // SBML (XML) subelements.
  //

  inline bool isEmpty ( const SBase_t*                    sb  );
  inline bool isEmpty ( const Model_t*                    m   );
  inline bool isEmpty ( const UnitDefinition_t*           ud  );
  inline bool isEmpty ( const Unit_t*                     u   );
  inline bool isEmpty ( const Compartment_t*              c   );
  inline bool isEmpty ( const Species_t*                  s   );
  inline bool isEmpty ( const Parameter_t*                p   );
  inline bool isEmpty ( const AlgebraicRule_t*            ar  );
  inline bool isEmpty ( const SpeciesConcentrationRule_t* scr );
  inline bool isEmpty ( const CompartmentVolumeRule_t*    cvr );
  inline bool isEmpty ( const ParameterRule_t*            pr  );
  inline bool isEmpty ( const Reaction_t*                 r   );
  inline bool isEmpty ( const SpeciesReference_t*         sr  );
  inline bool isEmpty ( const KineticLaw_t*               kl  );

  /**
   * Sends '<name>\n' to the underlying XMLFormatter.
   */
  inline void startElement (const XMLCh* name);

  /**
   * Sends '</name>\n' to the underlying XMLFormatter.
   */
  inline void endElement (const XMLCh* name);

  /**
   * Encapsulates a common operation for ending SBML (XML) elements that
   * contain non-empty <notes>, <annotation>s or both, but are not allowed
   * to contain other subelements like <listOfXXXs> or <kineticLaw>s.
   */
  inline void endElement (const XMLCh* name, const SBase_t* sb);

  /**
   * Sends '<name' to the underlying XMLFormatter.  Use when name has one or
   * more attributes.
   *
   * See also closeStartElement() or slashCloseStartElement().
   */
  inline void openStartElement (const XMLCh* name);

  /**
   * Sends '>\n' to the underlying XMLFormatter.
   *
   * See also openStartElement().
   */
  inline void closeStartElement ();

  /**
   * Sends "/>\n" to the underlying XMLFormatter.
   *
   * See also openStartElement().
   */
  inline void slashCloseStartElement ();

  //
  // Sends ' name=value' to the underlying XMLFormatter where value is an
  // appropriate string representation for the given type.
  //

  void attribute ( const XMLCh* name, bool         value );
  void attribute ( const XMLCh* name, int          value );
  void attribute ( const XMLCh* name, unsigned int value );
  void attribute ( const XMLCh* name, double       value );
  void attribute ( const XMLCh* name, const char*  value );
  void attribute ( const XMLCh* name, const XMLCh* value );

  /**
   * Sends whitespace to the underlying XMLFormatter based on the current
   * indentation level.
   */
  void indent ();

  inline void SBMLFormatter::upIndent   () { fIndentLevel++; }
  inline void SBMLFormatter::downIndent () { fIndentLevel--; }


  //
  // For the statement:
  //
  //   static const unsigned int NUMBER_BUFFER_SIZE = 100;
  //
  // MSVC++ 6.0 complains: "error C2258: illegal pure syntax, must be '=
  // 0'", but g++ has no problem with it?!  Fine.  For now, just #define.
  //
#define NUMBER_BUFFER_SIZE 100

  char fNumberBuffer[ NUMBER_BUFFER_SIZE ];

  unsigned int fLevel;
  unsigned int fVersion;

  XMLFormatter*     fFormatter;
  XMLFormatTarget*  fTarget;
  unsigned int      fIndentLevel;
};


#endif  // SBMLFormatter_hh
