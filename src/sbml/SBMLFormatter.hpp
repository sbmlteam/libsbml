/**
 * Filename    : SBMLFormatter.hpp
 * Description : Formats SBML ...
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
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
 *   Stefan Hoops
 */


#ifndef SBMLFormatter_hpp
#define SBMLFormatter_hpp


#include "common.hpp"


#ifndef USE_EXPAT
#  include <xercesc/framework/XMLFormatter.hpp>
#  include <xercesc/framework/MemBufFormatTarget.hpp>
#  include <xercesc/util/PlatformUtils.hpp>
#  include <xercesc/util/XMLString.hpp>
#  include <xercesc/util/XMLUniDefs.hpp>
#endif  // !USE_EXPAT


#include "List.h"
#include "SBMLTypes.hpp"

#include "MathMLFormatter.hpp" 
#include "SBMLUnicodeConstants.hpp" 


/**
 * SBMLFormatter is meant to act like a C++ output stream.  Creating an
 * SBMLFormatter requires a character encoding and an underlying
 * XMLFormatTarget, which can be either in-memory (with MemBufFormatTarget)
 * or file (FileFormatTarget), to be specified.  Once created, inserting
 * SBML objects (C structs) into the stream (with <<) will cause them to be
 * formatted in the character encoding for the XMLFormatTarget.
 *
 *
 * By default, the formatter outputs SBML Level 2, version 1 (L2v1).  To
 * change the default, either i) insert an SBMLDocument into the formatter
 * stream and the formatter will use the level and version of the given
 * SBMLDocument (most common) or ii) insert a level or version directly
 * (usually used unit test purposes) as in:
 *
 *   formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
 *
 * Currently, this class is meant to be used internally by libsbml.
 */
class SBMLFormatter
{

public:

  /**
   * Ctor
   *
   * Creates a new SBMLFormatter with the given character encoding.
   */
  SBMLFormatter (   const char*      outEncoding
                  , XMLFormatTarget* target
                  , bool             outputXMLDecl = true );

  /**
   * Dtor
   */
  ~SBMLFormatter ();


  enum SBMLLevel_t   { Level1   = 1, Level2   = 2 };
  enum SBMLVersion_t { Version1 = 1, Version2 = 2 };

  /**
   * Write the program identification comment
   */
  void writeComment(const char * programName,
                    const char * programVersion);

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

  SBMLFormatter& operator<< ( const SBMLDocument&             d   );
  SBMLFormatter& operator<< ( const Model&                    m   );
  SBMLFormatter& operator<< ( const FunctionDefinition&       fd  );
  SBMLFormatter& operator<< ( const UnitDefinition&           ud  );
  SBMLFormatter& operator<< ( const Unit&                     u   );
  SBMLFormatter& operator<< ( const Compartment&              c   );
  SBMLFormatter& operator<< ( const Species&                  s   );
  SBMLFormatter& operator<< ( const Parameter&                p   );
  SBMLFormatter& operator<< ( const Rule&                     r   );
  SBMLFormatter& operator<< ( const AssignmentRule&           ar  );
  SBMLFormatter& operator<< ( const RateRule&                 rr  );
  SBMLFormatter& operator<< ( const AlgebraicRule&            ar  );
  SBMLFormatter& operator<< ( const SpeciesConcentrationRule& scr );
  SBMLFormatter& operator<< ( const CompartmentVolumeRule&    cvr );
  SBMLFormatter& operator<< ( const ParameterRule&            pr  );
  SBMLFormatter& operator<< ( const Reaction&                 r   );
  SBMLFormatter& operator<< ( const SimpleSpeciesReference&   ssr );
  SBMLFormatter& operator<< ( const SpeciesReference&         sr  );
  SBMLFormatter& operator<< ( const ModifierSpeciesReference& msr );
  SBMLFormatter& operator<< ( const KineticLaw&               kl  );
  SBMLFormatter& operator<< ( const Event&                    e   );
  SBMLFormatter& operator<< ( const EventAssignment&          ea  );


private:

  void listOfFunctionDefinitions ( const ListOf& list );
  void listOfUnitDefinitions     ( const ListOf& list );
  void listOfUnits               ( const ListOf& list );
  void listOfCompartments        ( const ListOf& list );
  void listOfSpecies             ( const ListOf& list );
  void listOfParameters          ( const ListOf& list );
  void listOfRules               ( const ListOf& list );
  void listOfReactions           ( const ListOf& list );
  void listOfReactants           ( const ListOf& list );
  void listOfProducts            ( const ListOf& list );
  void listOfModifiers           ( const ListOf& list );
  void listOfEvents              ( const ListOf& list );
  void listOfEventAssignments    ( const ListOf& list );

  void annotation (const std::string& s);
  void notes      (const std::string& s);

  inline void notesAndAnnotation (const SBase& sb);

  /**
   * Outputs the <math> element for KineticLaw (L2 only).
   *
   * This method does the nescessary conversion if the KineticLaw has only
   * a formula string set.
   */
  void doMath (const KineticLaw& kl);

  /**
   * Outputs the <math> element for Rules (L2 only).
   *
   * This method does the nescessary conversion if the rule has only a
   * formula string set.
   */
  void doMath (const Rule& r);

  /**
   * Outputs the <stoichiometryMath> element for SpeciesReference (L2 only).
   */
  void doMath (const SpeciesReference& sr);

  /**
   * Outputs the metaid attribute for the given SBML object (L2 only).
   */
  void doMetaId (const SBase& sb);

  /**
   * Outputs the XML namespace attributes for the given SBML object.
   */
  void doXMLNS (const SBase& sb);

  /**
   * Outputs the type attribute for Rules (L1 only).
   */
  void doRuleType (const RuleType_t type);

  /**
   * @return true if the given Rule contains no child XML elements.
   */
  bool isEmpty (const Rule& r);

  /**
   * @return true if the given SpeciesReference contains no child XML
   * elements.
   */
  bool isEmpty (const SpeciesReference& sr);

  /**
   * @return true if the given KineticLaw contains no child XML elements.
   */
  bool isEmpty (const KineticLaw& kl);

  //
  // In this context "empty" means either no notes, annotations and other
  // SBML (XML) subelements.
  //

  inline bool isEmpty ( const SBase&                    sb  );
  inline bool isEmpty ( const SBMLDocument&             d   );
  inline bool isEmpty ( const Model&                    m   );
  inline bool isEmpty ( const FunctionDefinition&       fd  );
  inline bool isEmpty ( const UnitDefinition&           ud  );
  inline bool isEmpty ( const Unit&                     u   );
  inline bool isEmpty ( const Compartment&              c   );
  inline bool isEmpty ( const Species&                  s   );
  inline bool isEmpty ( const Parameter&                p   );
  inline bool isEmpty ( const Reaction&                 r   );
  inline bool isEmpty ( const ModifierSpeciesReference& msr );
  inline bool isEmpty ( const Event&                    e   );
  inline bool isEmpty ( const EventAssignment&          ea  );

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
  inline void endElement (const XMLCh* name, const SBase& sb);

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

  void attribute ( const XMLCh* name      , bool               value );
  void attribute ( const XMLCh* name      , int                value );
  void attribute ( const XMLCh* name      , unsigned int       value );
  void attribute ( const XMLCh* name      , double             value );
  void attribute ( const XMLCh* name, const std::string& value );
#ifndef USE_EXPAT
  void attribute ( const XMLCh* name, const char*  value );
#endif  // !USE_EXPAT
  void attribute ( const XMLCh* name, const XMLCh* value );

  /**
   * Sends whitespace to the underlying XMLFormatter based on the current
   * indentation level.
   */
  void indent ();

  inline void upIndent   () { fIndentLevel++; }
  inline void downIndent () { fIndentLevel--; }


  unsigned int fLevel;
  unsigned int fVersion;

  MathMLFormatter*  fMathFormatter;
  XMLFormatter*     fFormatter;
  XMLFormatTarget*  fTarget;

  char *fNumberBuffer;

  unsigned int      fIndentLevel;
};


#endif  // SBMLFormatter_hpp
