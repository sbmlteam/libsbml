/**
 * Filename    : LayoutHandler.h
 * Description : SBML Layout LayoutHandler Header
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


#ifndef LayoutHandler_h
#define LayoutHandler_h

#include "common/common.h"


#ifdef USE_EXPAT
#include "xml/Expat.h"
#include "xml/ExpatAttributes.h"
#else
#include <xercesc/sax/Locator.hpp>
#include <xercesc/sax2/DefaultHandler.hpp>
#endif /* USE_EXPAT */

#include "util/Stack.h"
#include "xml/ParseMessage.h"
#include "xml/XMLStringFormatter.h"

#include "sbml/SBMLUnicodeConstants.h"

#include "LayoutTagCodes.h"
#include "LayoutUnicodeConstants.h"
#include "Layout.h"


class SBMLHandler;

typedef SBase* (SBMLHandler::*TagHandler)(const Attributes& attrs);


#ifdef USE_EXPAT
typedef Expat DefaultHandler;
#endif /* USE_EXPAT */


/**
 * LayoutHandler
 *
 * This XML document handler is responsible for constructing an an Abstract
 * Syntax Tree from SAX2 events deliverd by a SAX2XMLReader.
*/
class LayoutHandler : public DefaultHandler
{
public:

  /**
   * Creates a new LayoutHandler.  The given MathMLDocument should be empty
   * and will be populated as the document is parsed.
   */
  LayoutHandler (ListOf *l);

  virtual ~LayoutHandler();
  
#ifdef USE_EXPAT
    virtual void onStartElement(const XML_Char *pszName,
                                const XML_Char **papszAttrs);

    virtual void onEndElement(const XML_Char *pszName);

    void onCharacterData(const XML_Char *chars,int length);
    
#else
  void startElement
  (
    const XMLCh* const  uri,
    const XMLCh* const  localname,
    const XMLCh* const  qname,
    const xercesc::Attributes&   attrs
  );

  void endElement
  (
    const XMLCh* const  uri,
    const XMLCh* const  localname,
    const XMLCh* const  qname
  );

  void characters(const XMLCh* const chars, const unsigned int length);
  

  void setDocumentLocator (const Locator *const locator);

  void warning (const SAXParseException&);
  void error   (const SAXParseException&);
  void fatalerror (const SAXParseException&);


  ParseMessage* ParseMessage_createFrom(const SAXParseException& e);

 
#endif /* USE_EXPAT */
  
void startDocument ();
void endDocument   ();
void ignorableWhitespace(const XMLCh* const chars, const unsigned int length);


void warning(const char* message);
void error(const char* message);
void fatalerror(const char* message);

ParseMessage* ParseMessage_createFrom(const char* message);

     
ListOf* getListOfLayouts();

private:


  LayoutTagCode_t getTagCode (const XMLCh* uri, const XMLCh* localname);


  ListOf*         listOfLayouts;
  Layout*         currentLayout;
  List*           warnings;
  List*           errors;
  List*           fatalErrors;
  Stack_t*          fObjStack;
  Stack_t*          fTagStack;

  int inNotes;
  int inAnnotation;
  int indent;
  
#ifndef USE_EXPAT
  const xercesc::Locator* fLocator;
#endif
  
  bool fSeenSep;
  XMLStringFormatter* fFormatter;
  

  SBase* doListOfLayouts (const Attributes& a);
  SBase* doLayout (const Attributes& a);
  SBase* doGraphicalObject(const Attributes& a);
  SBase* doCompartmentGlyph(const Attributes& a);
  SBase* doSpeciesGlyph(const Attributes& a);
  SBase* doReactionGlyph(const Attributes& a);
  SBase* doSpeciesReferenceGlyph(const Attributes& a);
  SBase* doTextGlyph(const Attributes& a);
  SBase* doListOfSpeciesReferenceGlyphs(const Attributes& a);
  SBase* doListOfSpeciesGlyphs(const Attributes& a);
  SBase* doListOfReactionGlyphs(const Attributes& a);
  SBase* doListOfCompartmentGlyphs(const Attributes& a);
  SBase* doListOfAdditionalGraphicalObjects(const Attributes& a);
  SBase* doCurve(const Attributes& a);
  SBase* doCurveSegment(const Attributes& a);
  SBase* doListOfCurveSegments(const Attributes& a);
  SBase* doLineSegment(const Attributes& a);
  SBase* doCubicBezier(const Attributes& a);
  SBase* doListOfTextGlyphs(const Attributes& a); 
  SBase* doDimensions(const Attributes& a);
  SBase* doBoundingBox(const Attributes& a);  
  SBase* doPosition(const Attributes& a);
  SBase* doStart(const Attributes& a);
  SBase* doEnd(const Attributes& a);  
  SBase* doBasePoint1(const Attributes& a);
  SBase* doBasePoint2(const Attributes& a);  
  
  void setLineAndColumn     (SBase *sb);
  
};


#endif  /* LayoutHandler_h */
