/**
 * Filename    : LayoutFormatter.h
 * Description : SBML Layout LayoutFormatter Header
 * Organization: European Media Laboratories Research gGmbH
 * Created     : 2004-04-26
 *
 * Copyright 2004 European Media Laboratories Research gGmbH
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
 * European Media Laboratories Research gGmbH have no obligations to
 * provide maintenance, support, updates, enhancements or modifications.
 * In no event shall the European Media Laboratories Research gGmbH be
 * liable to any party for direct, indirect, special, incidental or
 * consequential damages, including lost profits, arising out of the use of
 * this software and its documentation, even if the European Media
 * Laboratories Research gGmbH have been advised of the possibility of such
 * damage.  See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ralph Gauges
 *     Bioinformatics Group
 *     European Media Laboratories Research gGmbH
 *     Schloss-Wolfsbrunnenweg 31c
 *     69118 Heidelberg
 *     Germany
 *
 *     http://www.eml-research.de/english/Research/BCB/
 *     mailto:ralph.gauges@eml-r.villa-bosch.de
 *
 * Contributor(s):
 */


#ifndef LayoutFormatter_h
#define LayoutFormatter_h


#include "common/libsbml-config.h"
#include "xml/XMLUtil.h"


#ifdef USE_EXPAT
#  include "xml/ExpatFormatter.h"
#else
#  include <xercesc/framework/XMLFormatter.hpp>
#  include <xercesc/framework/MemBufFormatTarget.hpp>
#  include <xercesc/util/PlatformUtils.hpp>
#  include <xercesc/util/XMLString.hpp>
#  include <xercesc/util/XMLUniDefs.hpp>
#endif  /* USE_EXPAT */


#include "sbml/SBase.h"
#include "sbml/ListOf.h"

#include "CubicBezier.h"
#include "LineSegment.h"
#include "BoundingBox.h"
#include "Point.h"
#include "Dimensions.h"
#include "Curve.h"
#include "GraphicalObject.h"
#include "CompartmentGlyph.h"
#include "SpeciesGlyph.h"
#include "ReactionGlyph.h"
#include "SpeciesReferenceGlyph.h"
#include "TextGlyph.h"
#include "Layout.h"


/**
 * LayoutFormatter is meant to act like a C++ output stream.  Creating an
 * LayoutFormatter requires a character encoding and an underlying
 * XMLFormatTarget, which can be either in-memory (with MemBufFormatTarget)
 * or file (FileFormatTarget), to be specified.  Once created, inserting
 * ASTNode objects (C structs) into the stream (with <<) will cause them to
 * be formatted in the character encoding for the XMLFormatTarget.
 *
 * Currently, this class is meant to be used internally by libsbml.
*/
class LayoutFormatter
{
public:

  /**
   * Creates a new LayoutFormatter.  If outputXMLDecl is true the output
   * will begin with:
   *
   *   <?xml version="1.0" encoding="UTF-8"?>
   */
  LayoutFormatter (XMLFormatTarget* target, bool outputXMLDecl=true);
  
  /**
   * Destroys this LayoutFormatter.
   */
  ~LayoutFormatter ();

  /**
   * Sets the current indentation level for this LayoutFormatter.
   */
  void setIndentLevel (unsigned int n) { mIndentLevel = n; }


  void startLayout();

  void endLayout();
  void doListOfLayouts(const ListOf& l);

  

  /**
   * SBase insertion operator
   */
  LayoutFormatter& operator<< (const SBase& node);


private:


  /* ----------------------------------------------------------------------
   *                 Insertion Operator Supporting Functions
   * ----------------------------------------------------------------------
   */

    void doCompartmentGlyph(const CompartmentGlyph& cg);
    void doCurve(const Curve& c);
    void doCurveSegment(const LineSegment& ls);
    void doCubicBezier(const CubicBezier& cb);
    void doLineSegment(const LineSegment& ls);
    void doLayout(const Layout& l);
    void doListOfAdditionalGraphicalObjects(const ListOf& l);
    void doListOfCompartmentGlyphs(const ListOf& l);
    void doListOfCurveSegments(const ListOf& l);
    void doListOfReactionGlyphs(const ListOf& l);
    void doListOfSpeciesReferenceGlyphs(const ListOf& l);
    void doListOfSpeciesGlyphs(const ListOf& l);
    void doReactionGlyph(const ReactionGlyph& rg);
    void doSpeciesReferenceGlyph(const SpeciesReferenceGlyph& srg);
    void doListOfTextGlyphs(const ListOf& l);
    void doTextGlyph(const TextGlyph& tg);
    void doGraphicalObject(const GraphicalObject& go);
    void notesAndAnnotation(const SBase& sb);
    void notes(const char* s);
    void annotation(const char* s);
    void doMetaId (const SBase& sb);
    void doGraphicalObjectAttributes(const GraphicalObject& go);
    void doSpeciesGlyph(const SpeciesGlyph& sg); 
    void doId(const GraphicalObject& go);
    void doNameSpace();
    void doBoundingBox(const BoundingBox& bb);
    void doDimensions(const Dimensions& dim);
    void doPoint(const Point& p,const char*);
    void doPosition(const Point& p);
    void doLineSegmentStart(const Point& p);
    void doLineSegmentEnd(const Point& p);
    void doCubicBezierBasePoint1(const Point& p);
    void doCubicBezierBasePoint2(const Point& p);
    void doText(const TextGlyph& tg);
    void doOriginOfTextId(const TextGlyph& tg);
    void doGraphicalObjectId(const TextGlyph& tg);
    
    
  /* ----------------------------------------------------------------------
   *                      XML Elements and Attributes
   * ----------------------------------------------------------------------
   */


  void startElement(const XMLCh* name);

    
  /**
   * Sends '<name> ' to the underlying XMLFormatter.
   */
  void startElementSpace (const XMLCh* name);

  /**
   * Sends '</name>\n' to the underlying XMLFormatter.
   */
  void endElement (const XMLCh* name);

  /**
   * Sends ' </name>\n' to the underlying XMLFormatter.
   */
  void spaceEndElement (const XMLCh* name);

  /**
   * Sends '<name/>\n' to the underlying XMLFormatter.
   *
   * This is convenience function is equivalent to the following:
   *
   *  openStartElement(name);
   *  slashCloseStartElement(name);
   */
  inline void startEndElement (const XMLCh* name);

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
   * Sends '> ' to the underlying XMLFormatter.
   *
   * See also openStartElement().
   */
  inline void closeStartElementSpace ();

  /**
   * Sends "/>\n" to the underlying XMLFormatter.
   *
   * See also openStartElement().
   */
  inline void slashCloseStartElement ();

  /**
   * Returns true if the string pointed to by s is NULL or zero-length.
   */
  inline bool isEmpty (const char* s);

  /**
   * Returns true if the notes and annotations are empty.
   */
  bool isEmpty(const SBase& o);



  void attribute ( const XMLCh* name, bool         value );
  void attribute ( const XMLCh* name, int          value );
  void attribute ( const XMLCh* name, unsigned int value );
  void attribute ( const XMLCh* name, double       value );
#ifndef USE_EXPAT
  void attribute ( const XMLCh* name, const char*  value );
#endif  /* !USE_EXPAT */
  void attribute ( const XMLCh* name, const XMLCh* value );


  
  /**
   * Sends the given string of characters to the underlying XMLFormatter.
   */
#ifndef USE_EXPAT
  void characters (const char*  chars);
#endif  /* !USE_EXPAT */
  void characters (const XMLCh* chars);

  /**
   * Sends whitespace to the underlying XMLFormatter based on the current
   * indentation level.
   */
  void indent ();

  inline void upIndent   () { mIndentLevel++; }
  inline void downIndent () { mIndentLevel--; }


  /**
   * For the statement:
   *
   *   static const unsigned int NUMBER_BUFFER_SIZE = 100;
   *
   * MSVC++ 6.0 complains: "error C2258: illegal pure syntax, must be '=
   * 0'", but g++ has no problem with it?!  Fine.  For now, just #define.
   */
  static const unsigned int NUMBER_BUFFER_SIZE;

  char* mNumberBuffer;

  XMLFormatter*     mFormatter;
  unsigned int      mIndentLevel;
};


#endif  /* LayoutFormatter_h */
