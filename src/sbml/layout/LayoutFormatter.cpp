/**
 * Filename    : LayoutFormatter.cpp
 * Description : SBML Layout LayoutFormatter
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


#include "common/common.h"


#ifdef USE_EXPAT
#  include "xml/ExpatUnicodeChars.h"
#  include "xml/ExpatFormatter.h"
#  include "xml/ExpatXMLString.h"
#endif  // USE_EXPAT


#include "LayoutFormatter.h"
#include "LayoutUnicodeConstants.h"

#include "sbml/SBMLUnicodeConstants.h"
#include "xml/XMLUnicodeConstants.h"

#include "util/util.h"


const unsigned int
LayoutFormatter::NUMBER_BUFFER_SIZE = 100;


/**
 * Creates a new LayoutFormatter.  If outputXMLDecl is true the output
 * will begin with:
 *
 *   <?xml version="1.0" encoding="UTF-8"?>
 */
LayoutFormatter::LayoutFormatter (XMLFormatTarget* target, bool outputXMLDecl) :
    mIndentLevel ( 0 )
  , mNumberBuffer( new char[ NUMBER_BUFFER_SIZE ] )
{
#ifndef USE_EXPAT
  //
  // Initialize() is static and may be called more than once safely.
  //
  XMLPlatformUtils::Initialize();
#endif  // !USE_EXPAT

  mFormatter     = XMLUtil::createXMLFormatter("UTF-8", target);

  if (outputXMLDecl) *mFormatter << XML_DECL;
}


/**
 * Destroys this LayoutFormatter.
 */
LayoutFormatter::~LayoutFormatter ()
{
  delete mFormatter;
  delete[] mNumberBuffer;
}




/* ----------------------------------------------------------------------
 *                          Insertion operator
 * ----------------------------------------------------------------------
 */




/**
 * ASTNode insertion operator
 */
LayoutFormatter&
LayoutFormatter::operator<< (const SBase& node)
{
    FILE* errStream;
    SBMLTypeCode_t tcode;
    tcode=node.getTypeCode();
    switch(tcode){
        case SBML_LAYOUT_BOUNDINGBOX:
            doBoundingBox((const BoundingBox&)node);
            break;
        case SBML_LAYOUT_COMPARTMENTGLYPH:
            doCompartmentGlyph((const CompartmentGlyph&)node);
            break;
        case SBML_LAYOUT_CUBICBEZIER:
            doCubicBezier((const CubicBezier&)node);
            break;
        case SBML_LAYOUT_CURVE:
            doCurve((const Curve&)node);
            break;
        case SBML_LAYOUT_DIMENSIONS:
            doDimensions((const Dimensions&)node);
            break;    
        case SBML_LAYOUT_GRAPHICALOBJECT:
            doGraphicalObject((const GraphicalObject&)node);
            break;
        case SBML_LAYOUT_LAYOUT:
            doLayout((const Layout&)node);
            break;
        case SBML_LAYOUT_LINESEGMENT:
            doLineSegment((const LineSegment&)node);
            break;
        case SBML_LAYOUT_REACTIONGLYPH:
            doReactionGlyph((const ReactionGlyph&)node);
            break;
        case SBML_LAYOUT_SPECIESGLYPH:
            doSpeciesGlyph((const SpeciesGlyph&)node);
            break;
        case SBML_LAYOUT_SPECIESREFERENCEGLYPH:
            doSpeciesReferenceGlyph((const SpeciesReferenceGlyph&)node);
            break;
        case SBML_LAYOUT_TEXTGLYPH:
            doTextGlyph((const TextGlyph&)node);
            break;
        default:
            errStream=fdopen(2,"w");
            fprintf(errStream,"Error. Unknown tag %d.\n",tcode);
            fclose(errStream);
            exit(1);
            break;
    }

    return *this;
}



/* ----------------------------------------------------------------------
 *                 Insertion Operator Supporting Functions
 * ----------------------------------------------------------------------
 */



/* ----------------------------------------------------------------------
 *                      XML Elements and Attributes
 * ----------------------------------------------------------------------
 */

void LayoutFormatter::startLayout(){
    openStartElement(LIST_OF_LAYOUTS);
    attribute(ATTR_XMLNS,XMLNS_LAYOUT);
    closeStartElement();
    upIndent();
}

void LayoutFormatter::endLayout(){
    downIndent();
    indent();
    endElement(LIST_OF_LAYOUTS);
}

void LayoutFormatter::doCompartmentGlyph(const CompartmentGlyph& cg){
    openStartElement(COMPARTMENT_GLYPH);
    doMetaId((const SBase&)cg);
    doGraphicalObjectAttributes((const GraphicalObject&)cg);
    if(cg.isSetCompartmentId()){
        attribute(ATTR_COMPARTMENT,cg.getCompartmentId().c_str());
    }
    closeStartElement();
    upIndent();
    notesAndAnnotation( (const SBase&) cg );
    doBoundingBox(cg.getBoundingBox());
    downIndent();
    indent();
    endElement(COMPARTMENT_GLYPH);
}

void LayoutFormatter::doSpeciesGlyph(const SpeciesGlyph& sg){
    openStartElement(SPECIES_GLYPH);
    doMetaId(sg);
    doGraphicalObjectAttributes((const GraphicalObject&)sg);
    if(sg.isSetSpeciesId()){
        attribute(ATTR_SPECIES,sg.getSpeciesId().c_str());
    }
    closeStartElement();
    upIndent();
    notesAndAnnotation( (const SBase&) sg );
    doBoundingBox(sg.getBoundingBox()); 
    downIndent();
    indent();
    endElement(SPECIES_GLYPH);
}

void LayoutFormatter::doCurve(const Curve& c){
    openStartElement(CURVE);
    doMetaId((const SBase&)c);
    closeStartElement();
    upIndent();
    notesAndAnnotation( (const SBase&) c );
    doListOfCurveSegments(c.getListOfCurveSegments());
    downIndent();
    indent();
    endElement(CURVE);
}

void LayoutFormatter::doCubicBezier(const CubicBezier& cb){
    openStartElement(CURVE_SEGMENT);
    attribute(ATTR_XSI_TYPE,"CubicBezier");
    doMetaId((const SBase&)cb);
    closeStartElement();
    upIndent();
    notesAndAnnotation( (const SBase&) cb );
    doPoint(cb.getStart(),"start");
    doPoint(cb.getEnd(),"end");
    doPoint(cb.getBasePoint1(),"basePoint1");
    doPoint(cb.getBasePoint2(),"basePoint2");    
    downIndent();
    indent();
    endElement(CURVE_SEGMENT);
}

void LayoutFormatter::doLineSegment(const LineSegment& ls){
    openStartElement(CURVE_SEGMENT);
    attribute(ATTR_XSI_TYPE,"LineSegment");
    doMetaId((const SBase&)ls);
    closeStartElement();
    upIndent();
    notesAndAnnotation( (const SBase&) ls );
    doPoint(ls.getStart(),"start");
    doPoint(ls.getEnd(),"end");
    downIndent();
    indent();
    endElement(CURVE_SEGMENT);
}

void LayoutFormatter::doLayout(const Layout& l){
    openStartElement(LAYOUT);
    doMetaId((const SBase&)l);
    attribute(ATTR_ID,l.getId().c_str());
    closeStartElement();
    upIndent();
    notesAndAnnotation( (const SBase&) l );
    doDimensions(l.getDimensions());
    doListOfCompartmentGlyphs(l.getListOfCompartmentGlyphs());
    doListOfSpeciesGlyphs(l.getListOfSpeciesGlyphs());
    doListOfReactionGlyphs(l.getListOfReactionGlyphs());
    doListOfTextGlyphs(l.getListOfTextGlyphs());
    doListOfAdditionalGraphicalObjects(l.getListOfAdditionalGraphicalObjects());
    downIndent();
    indent();
    endElement(LAYOUT);
}

void LayoutFormatter::doReactionGlyph(const ReactionGlyph& rg){
    openStartElement(REACTION_GLYPH);
    doMetaId((const SBase&)rg);
    doGraphicalObjectAttributes((const GraphicalObject&)rg);
    if(rg.isSetReactionId()){
        attribute(ATTR_REACTION,rg.getReactionId().c_str());
    }
    closeStartElement();
    upIndent();
    notesAndAnnotation( (const SBase&) rg );
    if(rg.isSetCurve()){
        doCurve(*rg.getCurve());
    }
    else
    {
        doBoundingBox(rg.getBoundingBox());
    }
    doListOfSpeciesReferenceGlyphs(rg.getListOfSpeciesReferenceGlyphs());
    downIndent();
    indent();
    endElement(REACTION_GLYPH);
}

void LayoutFormatter::doSpeciesReferenceGlyph(const SpeciesReferenceGlyph& srg){
    openStartElement(SPECIES_REFERENCE_GLYPH);
    doMetaId((const SBase&)srg);
    doGraphicalObjectAttributes((const GraphicalObject&)srg);
    if(srg.isSetSpeciesReferenceId()){
        attribute(ATTR_SPECIES_REFERENCE,srg.getSpeciesReferenceId().c_str());
    }
    if(srg.isSetSpeciesGlyphId()){
        attribute(ATTR_SPECIES_GLYPH,srg.getSpeciesGlyphId().c_str());
    }
    attribute(ATTR_ROLE,srg.getRoleString().c_str());
    closeStartElement();
    upIndent();
    notesAndAnnotation( (const SBase&) srg );
    if(srg.isSetCurve()){
        doCurve(*srg.getCurve());
    }
    else
    {
        doBoundingBox(srg.getBoundingBox());
    }
    downIndent();
    indent();
    endElement(SPECIES_REFERENCE_GLYPH);
}

void LayoutFormatter::doTextGlyph(const TextGlyph& tg){
    openStartElement(TEXT_GLYPH);
    doMetaId((const SBase&)tg);
    doGraphicalObjectAttributes((const GraphicalObject&)tg);
    if(tg.isSetGraphicalObjectId()){
        doGraphicalObjectId(tg);
    }
    if(tg.isSetText()){
        doText(tg);
    }
    else if(tg.isSetOriginOfTextId()){
        doOriginOfTextId(tg);
    }
    closeStartElement();
    upIndent();
    notesAndAnnotation( (SBase&) tg );
    doBoundingBox(tg.getBoundingBox());
    downIndent();
    indent();
    endElement(TEXT_GLYPH);
}

void LayoutFormatter::doGraphicalObject(const GraphicalObject& go){
    openStartElement(GRAPHICAL_OBJECT);
    doMetaId((const SBase&)go);
    doGraphicalObjectAttributes(go);
    closeStartElement();
    upIndent();
    notesAndAnnotation( (SBase&) go );
    doBoundingBox(go.getBoundingBox());
    downIndent();
    indent();
    endElement(GRAPHICAL_OBJECT);
}

void LayoutFormatter::doListOfAdditionalGraphicalObjects(const ListOf& l){
    unsigned int x;
    if(l.getNumItems()!=0){
        openStartElement(LIST_OF_ADDITIONAL_GRAPHICAL_OBJECTS);
        doMetaId((const SBase&)l);
        closeStartElement();
        upIndent();
        notesAndAnnotation((const SBase&)l);
        for(x=0;x<l.getNumItems();x++){
            doGraphicalObject(*static_cast<const GraphicalObject*>(l.get(x)));
        }
        downIndent();
        indent();
        endElement(LIST_OF_ADDITIONAL_GRAPHICAL_OBJECTS);
    }
}

void LayoutFormatter::doListOfCompartmentGlyphs(const ListOf& l){
    unsigned int x;
    if(l.getNumItems()!=0){
        openStartElement(LIST_OF_COMPARTMENT_GLYPHS);
        doMetaId((const SBase&)l);
        closeStartElement();
        upIndent();
        notesAndAnnotation((SBase&)l);
        for(x=0;x<l.getNumItems();x++){
            doCompartmentGlyph(*static_cast<const CompartmentGlyph*>(l.get(x)));
        }
        downIndent();
        indent();
        endElement(LIST_OF_COMPARTMENT_GLYPHS);
    }
}

void LayoutFormatter::doListOfCurveSegments(const ListOf& l){
    unsigned int x;
    if(l.getNumItems()!=0){
        openStartElement(LIST_OF_CURVE_SEGMENTS);
        doMetaId((const SBase&)l);
        closeStartElement();
        upIndent();
        notesAndAnnotation((const SBase&)l);
        for(x=0;x<l.getNumItems();x++){
            doCurveSegment(*static_cast<const LineSegment*>(l.get(x)));
        }
        downIndent();
        indent();
        endElement(LIST_OF_CURVE_SEGMENTS);
    }
}

void LayoutFormatter::doNameSpace(){
    XMLCh* c=XMLString::transcode(" xmlns=\"http://projects.eml.org/bcb/sbml/level2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"");
    *mFormatter << c;
    XMLString::release(&c);
}

void LayoutFormatter::doListOfLayouts(const ListOf& l){
    unsigned int x;
    if(l.getNumItems()!=0){
        upIndent();
        openStartElement(LIST_OF_LAYOUTS);
        doMetaId((const SBase&)l);
        doNameSpace();
        closeStartElement();
        upIndent();
        notesAndAnnotation((const SBase&)l);
        for(int x=0;x<l.getNumItems();x++){
            doLayout(*static_cast<const Layout*>(l.get(x)));
        }
        downIndent();
        indent();
        endElement(LIST_OF_LAYOUTS);
        downIndent();
    }
}

void LayoutFormatter::doListOfReactionGlyphs(const ListOf& l){
    unsigned int x;
    if(l.getNumItems()!=0){
        openStartElement(LIST_OF_REACTION_GLYPHS);
        doMetaId((const SBase&)l);
        closeStartElement();
        upIndent();
        notesAndAnnotation((const SBase&)l);
        for(x=0;x<l.getNumItems();x++){
            doReactionGlyph(*static_cast<const ReactionGlyph*>(l.get(x)));
        }
        downIndent();
        indent();
        endElement(LIST_OF_REACTION_GLYPHS);
    }
}

void LayoutFormatter::doListOfSpeciesReferenceGlyphs(const ListOf& l){
    unsigned int x;
    if(l.getNumItems()!=0){
        openStartElement(LIST_OF_SPECIES_REFERENCE_GLYPHS);
        doMetaId((const SBase&)l);
        closeStartElement();
        upIndent();
        notesAndAnnotation((const SBase&)l);
        for(x=0;x<l.getNumItems();x++){
            doSpeciesReferenceGlyph(*static_cast<const SpeciesReferenceGlyph*>(l.get(x)));
        }
        downIndent();
        indent();
        endElement(LIST_OF_SPECIES_REFERENCE_GLYPHS);
    }
}

void LayoutFormatter::doListOfTextGlyphs(const ListOf& l){
    unsigned int x;
    if(l.getNumItems()!=0){
        openStartElement(LIST_OF_TEXT_GLYPHS);
        doMetaId((const SBase&)l);
        closeStartElement();
        upIndent();
        notesAndAnnotation((const SBase&)l);
        for(x=0;x<l.getNumItems();x++){
            doTextGlyph(*static_cast<const TextGlyph*>(l.get(x)));
        }
        downIndent();
        indent();
        endElement(LIST_OF_TEXT_GLYPHS);
    }
}

void LayoutFormatter::doListOfSpeciesGlyphs(const ListOf& l){
    unsigned int x;
    if(l.getNumItems()!=0){
        openStartElement(LIST_OF_SPECIES_GLYPHS);
        doMetaId((const SBase&)l);
        closeStartElement();
        upIndent();
        notesAndAnnotation((const SBase&)l);
        for(x=0;x<l.getNumItems();x++){
            doSpeciesGlyph(*static_cast<const SpeciesGlyph*>(l.get(x)));
        }
        downIndent();
        indent();
        endElement(LIST_OF_SPECIES_GLYPHS);
    }
}

void LayoutFormatter::doCurveSegment(const LineSegment& ls){
    SBMLTypeCode_t tc=ls.getTypeCode();
    if(tc==SBML_LAYOUT_CUBICBEZIER){
        doCubicBezier((const CubicBezier&)ls);
    }
    else{
        doLineSegment(ls);
    }
}

void LayoutFormatter::doBoundingBox(const BoundingBox& bb){
    openStartElement(BOUNDING_BOX);
    doMetaId((const SBase&)bb);
    if(bb.isSetId()){
       attribute(ATTR_ID,bb.getId().c_str()); 
    }
    closeStartElement();
    upIndent();
    notesAndAnnotation( (const SBase&) bb );
    doPoint(bb.getPosition(),"position");
    doDimensions(bb.getDimensions());
    downIndent();
    indent();
    endElement(BOUNDING_BOX);
}


void LayoutFormatter::doDimensions(const Dimensions& d){
    openStartElement(DIMENSIONS);
    doMetaId((const SBase&)d);
    attribute(ATTR_WIDTH,d.getWidth());
    attribute(ATTR_HEIGHT,d.getHeight());
    if(d.getDepth()!=0.0){
      attribute(ATTR_DEPTH,d.getDepth());
    }
    if(isEmpty(static_cast<const SBase&>(d))){
        slashCloseStartElement();
    }
    else{
        closeStartElement();
        upIndent();
        notesAndAnnotation( (const SBase&) d );
        downIndent();
        indent();
        endElement(DIMENSIONS);
    }
}

void LayoutFormatter::doPoint(const Point& p,const char* elemName){
    XMLCh* elementNameXMLCh=XMLString::transcode(elemName);
    openStartElement(elementNameXMLCh);
    doMetaId((const SBase&)p);
    attribute(ATTR_X,p.getXOffset());
    attribute(ATTR_Y,p.getYOffset());
    if(p.getZOffset()!=0.0){
      attribute(ATTR_Z,p.getZOffset());
    }
    if(isEmpty(static_cast<const SBase&>(p))){
        slashCloseStartElement();
    }
    else{
        closeStartElement();
        upIndent();
        notesAndAnnotation( (const SBase&) p );
        downIndent();
        indent();
        endElement(elementNameXMLCh);
    }
    XMLString::release(&elementNameXMLCh);
}


/**
 * Sends '<name>\n' to the underlying XMLFormatter.
 */
void
LayoutFormatter::startElement (const XMLCh* name)
{
  indent();
  *mFormatter << XMLFormatter::NoEscapes
              << chOpenAngle << name << chCloseAngle << chLF;
}



/**
 * Sends '<name> ' to the underlying XMLFormatter.
 */
void
LayoutFormatter::startElementSpace (const XMLCh* name)
{
  indent();
  *mFormatter << XMLFormatter::NoEscapes
              << chOpenAngle << name << chCloseAngle << chSpace;
}


/**
 * Sends '</name>\n' to the underlying XMLFormatter.
 */
void
LayoutFormatter::endElement (const XMLCh* name)
{
  *mFormatter << XMLFormatter::NoEscapes
              << chOpenAngle  << chForwardSlash << name << chCloseAngle
              << chLF;
}


/**
 * Sends ' </name>\n' to the underlying XMLFormatter.
 */
void
LayoutFormatter::spaceEndElement (const XMLCh* name)
{
  *mFormatter << XMLFormatter::NoEscapes
              << chSpace
              << chOpenAngle  << chForwardSlash << name << chCloseAngle
              << chLF;
}


/**
 * Sends '<name/>\n' to the underlying XMLFormatter.
 *
 * This is convenience function is equivalent to the following:
 *
 *  openStartElement(name);
 *  slashCloseStartElement(name);
 */
void
LayoutFormatter::startEndElement (const XMLCh* name)
{
  indent();
  *mFormatter << XMLFormatter::NoEscapes
              << chOpenAngle  << name << chForwardSlash
              << chCloseAngle << chLF;
}


/**
 * Sends '<name' to the underlying XMLFormatter.  Use when name has one or
 * more attributes.
 *
 * See also closeStartElement() or slashCloseStartElement().
 */
void
LayoutFormatter::openStartElement (const XMLCh* name)
{
  indent();
  *mFormatter << XMLFormatter::NoEscapes << chOpenAngle << name;
}


/**
 * Sends '>\n' to the underlying XMLFormatter.
 *
 * See also openStartElement().
 */
void
LayoutFormatter::closeStartElement ()
{
  *mFormatter << XMLFormatter::NoEscapes << chCloseAngle << chLF;
}


/**
 * Sends '> ' to the underlying XMLFormatter.
 *
 * See also openStartElement().
 */
void
LayoutFormatter::closeStartElementSpace ()
{
  *mFormatter << XMLFormatter::NoEscapes << chCloseAngle << chSpace;
}


/**
 * Sends "/>\n" to the underlying XMLFormatter.
 *
 * See also openStartElement().
 */
void
LayoutFormatter::slashCloseStartElement ()
{
  *mFormatter << XMLFormatter::NoEscapes
              << chForwardSlash << chCloseAngle << chLF;
}

#ifndef USE_EXPAT
/**
 * Sends the given string of characters to the underlying XMLFormatter.
 */
void
LayoutFormatter::characters (const char* chars)
{
  if (chars == NULL) return;

  XMLCh* s = XMLString::transcode(chars);
  characters(s);

  XMLString::release(&s);
}
#endif  // !USE_EXPAT


/**
 * Sends the given string of Unicode characters to the underlying
 * XMLFormatter.
 */
void
LayoutFormatter::characters (const XMLCh* chars)
{
  *mFormatter << XMLFormatter::CharEscapes << chars;
}

/**
 * Sends whitespace to the underlying XMLFormatter based on the current
 * indentation level.
 */
void
LayoutFormatter::indent ()
{
  for (unsigned int n = 0; n < mIndentLevel; n++)
  {
    *mFormatter << chSpace << chSpace;
  }
}


/**
 * Notes
 */
void
LayoutFormatter::notes (const char* s)
{
  if (isEmpty(s)) return;


  startElement(ELEM_NOTES);

  upIndent();
  indent();

  XMLCh* x = XMLString::transcode(s);
  *mFormatter << x << chLF;
  XMLString::release(&x);

  downIndent();

  endElement(ELEM_NOTES);
}


/**
 * Annotation
 */
void
LayoutFormatter::annotation (const char* s)
{
  if (isEmpty(s)) return;


  indent();

  XMLCh* x = XMLString::transcode(s);
  *mFormatter << x << chLF;
  XMLString::release(&x);
}


/**
 * Notes and Annotation
 */
void
LayoutFormatter::notesAndAnnotation(const SBase& sb)
{
  notes(sb.getNotes().c_str());
  annotation(sb.getAnnotation().c_str());
}

/**
 * Outputs the metaid attribute for the given SBML object (L2 only).
 */
void
LayoutFormatter::doMetaId (const SBase& sb)
{
    if (sb.isSetMetaId() )
    {
        attribute(ATTR_META_ID, sb.getMetaId().c_str());
    }
}

void LayoutFormatter::doId(const GraphicalObject& go){
    attribute(ATTR_ID,go.getId().c_str());
}

void LayoutFormatter::doText(const TextGlyph& tg){
    attribute(ATTR_TEXT,tg.getText().c_str());
}

void LayoutFormatter::doOriginOfTextId(const TextGlyph& tg){
    attribute(ATTR_ORIGIN_OF_TEXT_ID,tg.getOriginOfTextId().c_str());
}

void LayoutFormatter::doGraphicalObjectId(const TextGlyph& tg){
    attribute(ATTR_GRAPHICAL_OBJECT_ID,tg.getGraphicalObjectId().c_str());
}



void LayoutFormatter::doGraphicalObjectAttributes(const GraphicalObject& go){
    doId(go);
}



/**
 * Sends ' name="true"' or ' name="false"' to the underlying XMLFormatter
 */
void
LayoutFormatter::attribute (const XMLCh* name, bool value)
{
  (value == true) ? attribute(name, VAL_TRUE) : attribute(name, VAL_FALSE);
}


/**
 * Sends ' name="%d" to the underlying XMLFormatter (where %d is an integer).
 */
void
LayoutFormatter::attribute (const XMLCh* name, int value)
{
  snprintf(mNumberBuffer, NUMBER_BUFFER_SIZE, "%d", value);
  attribute(name, mNumberBuffer);
}


/**
 * Sends ' name="%u" to the underlying XMLFormatter (where %u is an unsigned
 * integer).
 */
void
LayoutFormatter::attribute (const XMLCh* name, unsigned int value)
{
  snprintf(mNumberBuffer, NUMBER_BUFFER_SIZE, "%u", value);
  attribute(name, mNumberBuffer);
}


/**
 * Sends ' name="%g" to the underlying XMLFormatter (where %g is a double).
 */
void
LayoutFormatter::attribute (const XMLCh* name, double value)
{
  if (value != value)
  {
    attribute(name, VAL_NAN);
  }
  else if ( util_isInf(value) == 1)
  {
    attribute(name, VAL_INF);
  }
  else if ( util_isInf(value) == -1)
  {
    attribute(name, VAL_NEG_INF);
  }
  else if ( util_isNegZero(value) )
  {
    attribute(name, VAL_NEG_ZERO);
  }
  else
  {
    snprintf(mNumberBuffer, NUMBER_BUFFER_SIZE, "%g", value);
    attribute(name, mNumberBuffer);
  }
}


#ifndef USE_EXPAT
/**
 * Sends ' name="%s" to the underlying XMLFormatter (where %s is a C string).
 */
void
LayoutFormatter::attribute (const XMLCh* name, const char* value)
{
  XMLCh* s;


  if (value == NULL)
  {
    attribute(name, (const XMLCh*) NULL);
  }
  else
  {
    s = XMLString::transcode(value);
    attribute(name, s);

    delete [] s;
  }
}
#endif // !USE_EXPAT


/**
 * Sends ' name="%s" to the underlying XMLFormatter (where %s is a Unicode
 * string).
 */
void
LayoutFormatter::attribute (const XMLCh* name, const XMLCh* value)
{
  *mFormatter
    << XMLFormatter::NoEscapes
    << chSpace
    << name
    << chEqual
    << chDoubleQuote
    << XMLFormatter::AttrEscapes;

  if (value != NULL)
  {
    *mFormatter << value;
  }

  *mFormatter << XMLFormatter::NoEscapes << chDoubleQuote;
}

/**
 * Returns true if the string pointed to by s is NULL or zero-length.
 */
bool
LayoutFormatter::isEmpty (const char* s)
{
  return !(s && *s);
}

/**
 * Returns true if the notes and annotations are empty.
 */
bool LayoutFormatter::isEmpty(const SBase& o){
    return (o.getNotes().empty() && o.getAnnotation().empty());    
}

