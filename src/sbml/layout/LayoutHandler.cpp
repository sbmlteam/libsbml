/**
 * Filename    : LayoutHandler.c
 * Description : SBML Layout LayoutHandler
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


#include <iostream>
#include <ctype.h>

#include "common/common.h"
#include "LayoutHandler.h"


#ifdef USE_EXPAT
#  include <string>
#  include "xml/ExpatXMLString.h"
#else
#  include <xercesc/sax2/Attributes.hpp>
#  include <xercesc/util/XMLString.hpp>
#endif  // USE_EXPAT

#include "util/List.h"
#include "xml/XMLUtil.h"
 
#include "sbml/SBMLHandler.h"

#include "GraphicalObject.h"
#include "SpeciesGlyph.h"
#include "CompartmentGlyph.h"
#include "ReactionGlyph.h"
#include "SpeciesReferenceGlyph.h"
#include "TextGlyph.h"
#include "BoundingBox.h"
#include "Point.h"
#include "Dimensions.h"
#include "LineSegment.h"
#include "CubicBezier.h"
#include "SpeciesReferenceRole.h"


using namespace std;


void
LayoutHandler::startElement (const XMLCh* const  uri,
                             const XMLCh* const  localname,
                             const XMLCh* const  qname,
                             const Attributes&   attrs)
{
  LayoutTagCode_t currTag  = getTagCode(uri, localname);
  LayoutTagCode_t prevTag  = TAG_UNKNOWN;
  FILE* errStream;
  SBase* obj=NULL;
  SBase* prevNode=NULL;

  /*
  char* lname = XMLString::transcode(localname);
  
  for (int counter=0;counter<this->indent;counter++){
	  std::cout << " ";
  }
  this->indent=this->indent+3;
  std::cout <<  "<" << lname << ">" << std::endl;
  XMLString::release(&lname);
  */

  if (inAnnotation)
  {
    fFormatter->startElement(qname, attrs);
    if (currTag == TAG_ANNOTATION || currTag == TAG_ANNOTATIONS)
    {
      inAnnotation++;
    }
  }
  else if (inNotes)
  {
    fFormatter->startElement(qname, attrs);
    if (currTag == TAG_NOTES)
    {
      warning("<notes> elements cannot be nested.");
      inNotes++;
    }
  }
  else if (currTag == TAG_ANNOTATION || currTag == TAG_ANNOTATIONS)
  {
    fFormatter->startElement(qname, attrs);
    inAnnotation++;
  }
  else if (currTag == TAG_NOTES)
  {
    inNotes++;
  }    
  else
  {
    if (Stack_size(fTagStack) > 0)
    {
      prevTag  = (LayoutTagCode_t) Stack_peek(fTagStack);
      prevNode = (SBase*)      Stack_peek(fObjStack);
    }

    switch (currTag)
    {
      case TAG_BOUNDING_BOX:
        obj=(SBase*)doBoundingBox(attrs);
        break;

      case TAG_COMPARTMENT_GLYPH:
        obj=(SBase*)doCompartmentGlyph(attrs);
        break;

      case TAG_CURVE:
        obj=(SBase*)doCurve(attrs);
        break;

      case TAG_CURVE_SEGMENT:
        obj=(SBase*)doCurveSegment(attrs);
        break;

      case TAG_DIMENSIONS:
        obj=(SBase*)doDimensions(attrs);
        break;      

      case TAG_LAYOUT:
        obj=(SBase*)doLayout(attrs);
        break;

      case TAG_LIST_OF_ADDITIONAL_GRAPHICAL_OBJECTS:
        obj=(SBase*)doListOfAdditionalGraphicalObjects(attrs);
        break;

      case TAG_LIST_OF_COMPARTMENT_GLYPHS:
        obj=(SBase*)doListOfCompartmentGlyphs(attrs);
        break;

      case TAG_LIST_OF_CURVE_SEGMENTS:
        obj=(SBase*)doListOfCurveSegments(attrs);
        break;

      case TAG_LIST_OF_LAYOUTS:
        obj=(SBase*)doListOfLayouts(attrs);
        break;

      case TAG_LIST_OF_REACTION_GLYPHS:
        obj=(SBase*)doListOfReactionGlyphs(attrs);
        break;

      case TAG_LIST_OF_SPECIES_GLYPHS:
        obj=(SBase*)doListOfSpeciesGlyphs(attrs);
        break;

      case TAG_LIST_OF_SPECIES_REFERENCE_GLYPHS:
        obj=(SBase*)doListOfSpeciesReferenceGlyphs(attrs);
        break;

      case TAG_LIST_OF_TEXT_GLYPHS:
        obj=(SBase*)doListOfTextGlyphs(attrs);
        break;

      case TAG_BOUNDING_BOX_POSITION:
        obj=(SBase*)doPosition(attrs);
        break;

      case TAG_LINE_SEGMENT_START:
        obj=(SBase*)doStart(attrs);
        break;

      case TAG_LINE_SEGMENT_END:
        obj=(SBase*)doEnd(attrs);
        break;

      case TAG_CUBIC_BEZIER_BASE_POINT_1:
        obj=(SBase*)doBasePoint1(attrs);
        break;

      case TAG_CUBIC_BEZIER_BASE_POINT_2:
        obj=(SBase*)doBasePoint2(attrs);
        break;    

      case TAG_REACTION_GLYPH:
        obj=(SBase*)doReactionGlyph(attrs);
        break;

      case TAG_SPECIES_GLYPH:
        obj=(SBase*)doSpeciesGlyph(attrs);
        break;

      case TAG_SPECIES_REFERENCE_GLYPH:
        obj=(SBase*)doSpeciesReferenceGlyph(attrs);
        break;

      case TAG_TEXT_GLYPH:
        obj=(SBase*)doTextGlyph(attrs);
        break;

      default:
        errStream=fdopen(2,"w");
        char* c=XMLString::transcode(qname);
        fprintf(errStream,"Error. Unknown start tag %s.\n",c);
        XMLString::release(&c);
        fclose(errStream);
        exit(1);
        break;
    }

    if (obj != NULL)
    {
      setLineAndColumn(obj);
      Stack_push(fTagStack, (void*) currTag);
      Stack_push(fObjStack, obj);
    }
    else
    {
      std::cout << "Sorry object is NULL." << std::endl;
    }
  }
}

   
void
LayoutHandler::endElement (const XMLCh* const  uri,
                           const XMLCh* const  localname,
                           const XMLCh* const  qname)
{
  LayoutTagCode_t prevTag;
  SBase* node;
  SBase* prevNode;
  LayoutTagCode_t tag  = getTagCode(uri, localname);
  FILE* errStream;
  if (tag == TAG_NOTES)
  {
    node=(SBase*)Stack_peek(fObjStack);
    if (inNotes > 1)
    {
      fFormatter->endElement(qname);
    }
    else if (inNotes == 1)
    {
      node->setNotes(fFormatter->getString());
      fFormatter->reset();
    }

    inNotes--;
  }

  //
  // Annotation
  //
  else if (tag == TAG_ANNOTATION || tag == TAG_ANNOTATIONS)
  {

    node=(SBase*)Stack_peek(fObjStack);
    fFormatter->endElement(qname);

    if (inAnnotation == 1)
    {
      node->setAnnotation(fFormatter->getString());
      fFormatter->reset();
    }

    inAnnotation--;
  }
  else
  {
    node = (SBase*) Stack_pop(fObjStack);
    prevNode=(SBase*)Stack_peek(fObjStack);
    Stack_pop(fTagStack);
    prevTag=(LayoutTagCode_t)Stack_peek(fTagStack);
      GraphicalObject* go;
      CubicBezier* cb;
      LineSegment* ls;
      switch (tag)
      {
          case TAG_BOUNDING_BOX:
              /* check if prevNode is a GraphicalObject */
              if(!dynamic_cast<GraphicalObject*>(prevNode))
              {
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. BoundingBox not inside a GraphicalObject.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_COMPARTMENT_GLYPH:
              if(prevTag!=TAG_LIST_OF_COMPARTMENT_GLYPHS){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. CompartmentGlyph not inside ListOfCompartments.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_CURVE:
              if(!(prevTag==TAG_SPECIES_REFERENCE_GLYPH || prevTag==TAG_REACTION_GLYPH)){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. Curve does not belong to a SpeciesReferenceGlyph.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_CURVE_SEGMENT:
              if(prevTag!=TAG_LIST_OF_CURVE_SEGMENTS){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. CurveSegment does not belong to a listOfCurveSegments.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_DIMENSIONS:
              /* either prevNode is Layout or BoundingBox */
              if(!(prevTag==TAG_BOUNDING_BOX || prevTag==TAG_LAYOUT)){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. Dimensions not inside Layout or BoundingBox.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;    
          case TAG_LAYOUT:
              if(prevTag!=TAG_LIST_OF_LAYOUTS){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. Layout not inside listOfLayouts.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_LIST_OF_ADDITIONAL_GRAPHICAL_OBJECTS:
              if(prevTag!=TAG_LAYOUT){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. ListOfAdditionalGraphicalObujects not inside a layout.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_LIST_OF_COMPARTMENT_GLYPHS:
              if(prevTag!=TAG_LAYOUT){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. ListOfCompartmentGlyphs not inside a layout.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_LIST_OF_CURVE_SEGMENTS:
              if(prevTag!=TAG_CURVE){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. ListOfCurveSegments not inside a curve.\n");
                  fprintf(errStream,"Tag: %d != %d\n",prevTag,TAG_CURVE);
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_LIST_OF_LAYOUTS:
              break;
          case TAG_LIST_OF_REACTION_GLYPHS:
              if(prevTag!=TAG_LAYOUT){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. ListOfReactionGlyphs not inside a layout.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_LIST_OF_SPECIES_GLYPHS:
              if(prevTag!=TAG_LAYOUT){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. ListOfSpeciesGlyphs not inside a layout.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_BOUNDING_BOX_POSITION:
              /* prevNode is BoundingBox*/
              if(prevTag!=TAG_BOUNDING_BOX){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. Position not inside BoundingBox.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_LINE_SEGMENT_START:
              /* prevNode is a LineSegment */
              ls=dynamic_cast<LineSegment*>(prevNode);
              if(!ls){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. Start does not belong to a LineSegment.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;     
          case TAG_LINE_SEGMENT_END:
              /* prevNode is a LineSegment */
              ls=dynamic_cast<LineSegment*>(prevNode);
              if(!ls){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. End does not belong to a LineSegment.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;     
          case TAG_CUBIC_BEZIER_BASE_POINT_1:
              /* prevNode is a CubicBezier */
              cb=dynamic_cast<CubicBezier*>(prevNode);
              if(!cb){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. BasePoint1 does not belong to a CubicBezier.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;     
          case TAG_CUBIC_BEZIER_BASE_POINT_2:
              /* prevNode is a CubicBezier */
              cb=dynamic_cast<CubicBezier*>(prevNode);
              if(!cb){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. BasePoint2 does not belong to a CubicBezier.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;     
          case TAG_SPECIES_GLYPH:
              if(prevTag!=TAG_LIST_OF_SPECIES_GLYPHS){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. SpeciesGlyph not inside listOfSpeciesGlyphs.\n");
		  fprintf(errStream,"Tag: %d\n",prevTag);
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_LIST_OF_SPECIES_REFERENCE_GLYPHS:
              if(prevTag!=TAG_REACTION_GLYPH){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. ListOfSpeciesReferenceGlyphs not inside a reactionGlyph.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_LIST_OF_TEXT_GLYPHS:
              if(prevTag!=TAG_LAYOUT){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. ListOfTextGlyphs not inside a layout.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_REACTION_GLYPH:
              if(prevTag!=TAG_LIST_OF_REACTION_GLYPHS){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. ReactionGlyph not inside listOfReactionGlyphs.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_SPECIES_REFERENCE_GLYPH:
              if(prevTag!=TAG_LIST_OF_SPECIES_REFERENCE_GLYPHS){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. SpeciesReferenceGlyph not inside listOfSpeciesReferenceGlyphs.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          case TAG_TEXT_GLYPH:
              if(prevTag!=TAG_LIST_OF_TEXT_GLYPHS){
                  errStream=fdopen(2,"w");
                  fprintf(errStream,"Error. TextGlyph not inside listOfTextGlyphs.\n");
                  fclose(errStream);
                  exit(1);
              }
              break;
          default:
              errStream=fdopen(2,"w");
              char* c=XMLString::transcode(qname);
              fprintf(errStream,"Error. Unknown end tag %s.\n",c);
              XMLString::release(&c);
              fclose(errStream);
              exit(1);
              break;
      }
    }
}
    

void
LayoutHandler::characters (const XMLCh* const  chars,
                           const unsigned int  length)
{
#ifdef USE_EXPAT
  if (XMLString::isAllWhiteSpace(chars, length) == false)
  {
    char* s = XMLString::transcode(chars, length);
#else
  if (XMLString::isAllWhiteSpace(chars) == false)
  {
    char* s = XMLString::transcode(chars);
#endif  // USE_EXPAT
    XMLString::release(&s);
  }
}


void
LayoutHandler::warning (const char* message)
{
  List_add( this->warnings, ParseMessage_createFrom(message) );
}

void
LayoutHandler::error (const char* message)
{
  List_add( this->errors, ParseMessage_createFrom(message) );
}

void
LayoutHandler::fatalerror (const char* message)
{
  List_add( this->fatalErrors, ParseMessage_createFrom(message) );
}


#ifndef USE_EXPAT

void 
LayoutHandler::warning (const SAXParseException& e)
{
  List_add( this->warnings, ParseMessage_createFrom(e) );
}

void
LayoutHandler::error (const SAXParseException& e)
{
  List_add( this->errors, ParseMessage_createFrom(e) );
}

void
LayoutHandler::fatalerror (const SAXParseException& e)
{
  List_add( this->fatalErrors, ParseMessage_createFrom(e) );
}

#endif  /* USE_EXPAT */



void
LayoutHandler::startDocument ()
{
  fObjStack = Stack_create(7);
  fTagStack = Stack_create(7);
  fSeenSep  = false;
  inNotes      = 0;
  inAnnotation = 0;
  fFormatter = new XMLStringFormatter("ASCII");
  indent=0;
}

void
LayoutHandler::endDocument ()
{
  if (Stack_size(fObjStack) > 0)
  {
    this->listOfLayouts = (ListOf*)Stack_pop(fObjStack);
  }
  Stack_free( fObjStack );
  Stack_free( fTagStack );
}



#ifndef USE_EXPAT
void
LayoutHandler::setDocumentLocator (const Locator *const locator)
{
  fLocator = locator;
}
#endif  /* USE_EXPAT */


void 
LayoutHandler::ignorableWhitespace (const XMLCh* const chars,
                                    const unsigned int length)
{
}


/**
 * @return the LayoutTagCode for the given namespace URI and element name
 * (localname).
 */
LayoutTagCode_t
LayoutHandler::getTagCode (const XMLCh *uri, const XMLCh* localname)
{
  unsigned int  len = XMLString::stringLen(uri);
  SBMLTagCode_t tag = TAG_UNKNOWN;
  
  XMLCh ch;


  if (len == 0)
  {
    tag = LayoutTagCode_forElement(localname);
  }
  else
  {
      tag = LayoutTagCode_forElement(localname);
  }

  return tag;
}



LayoutHandler::LayoutHandler (ListOf* l) : DefaultHandler(), listOfLayouts(l)
{
  this->errors     = new List();
  this->warnings   = new List();
  this->fatalErrors= new List();
}


LayoutHandler::~LayoutHandler()
{
  delete this->errors;
  delete this->warnings;
  delete this->fatalErrors;
}


SBase*
LayoutHandler::doListOfLayouts (const Attributes& a)
{
  return (SBase*)listOfLayouts;
}


SBase*
LayoutHandler::doLayout (const Attributes& a)
{
  Layout* l=new Layout();
    
  std::string id;
    
  XMLUtil::scanAttr(a, ATTR_ID, id);
  l->setId(id);
  currentLayout=l;
  listOfLayouts->append(currentLayout);
  return (SBase*)l;
}


SBase*
LayoutHandler::doGraphicalObject(const Attributes& a)
{
  GraphicalObject* go=currentLayout->createAdditionalGraphicalObject();
    
  XMLUtil::scanAttr(a, ATTR_ID, go->id);
  return (SBase*)go;
}


SBase*
LayoutHandler::doCompartmentGlyph(const Attributes& a)
{
  CompartmentGlyph* cg=currentLayout->createCompartmentGlyph();
   
    
  XMLUtil::scanAttr(a, ATTR_ID, cg->id);
  XMLUtil::scanAttr(a, ATTR_COMPARTMENT_ID, cg->compartment);
   
  return (SBase*)cg;
}


SBase*
LayoutHandler::doSpeciesGlyph(const Attributes& a){
    SpeciesGlyph* sg=currentLayout->createSpeciesGlyph();
   
    XMLUtil::scanAttr(a, ATTR_ID, sg->id);
    XMLUtil::scanAttr(a, ATTR_SPECIES_ID, sg->species);
    return (SBase*)sg;
}

SBase*
LayoutHandler::doReactionGlyph(const Attributes& a){
    ReactionGlyph* rg=currentLayout->createReactionGlyph();
    
    
    XMLUtil::scanAttr(a, ATTR_ID, rg->id);
    XMLUtil::scanAttr(a, ATTR_REACTION_ID, rg->reaction);
   
    return (SBase*)rg;

}

SBase*
LayoutHandler::doTextGlyph(const Attributes& a){
    TextGlyph* tg=currentLayout->createTextGlyph();
    std::string s; 
    
    
    XMLUtil::scanAttr(a, ATTR_ID, tg->id);
    
    if (XMLUtil::scanAttr(a, ATTR_ORIGIN_OF_TEXT_ID, s) == true)
    {
        tg->setOriginOfTextId(s);
    }
    if (XMLUtil::scanAttr(a, ATTR_TEXT, s) == true)
    {
        tg->setOriginOfTextId("");
        tg->setText(s);
    }
    if (XMLUtil::scanAttr(a, ATTR_GRAPHICAL_OBJECT_ID, s) == true)
    {
        tg->setGraphicalObjectId(s);
    }
    return (SBase*)tg;
}

SBase*
LayoutHandler::doListOfSpeciesReferenceGlyphs(const Attributes& a){
  SBase* obj=static_cast<SBase*>(Stack_peek(fObjStack));
  ListOf* l=NULL;
  ReactionGlyph* rg;

    if(obj->getTypeCode() == SBML_LAYOUT_REACTIONGLYPH)
    {
        rg=static_cast<ReactionGlyph*>(obj);
        l=&rg->getListOfSpeciesReferenceGlyphs();
    }
    return (SBase*)l;
}

SBase*
LayoutHandler::doListOfSpeciesGlyphs(const Attributes& a){
    return & currentLayout->getListOfSpeciesGlyphs();
}

SBase*
LayoutHandler::doListOfReactionGlyphs(const Attributes& a){
    return & currentLayout->getListOfReactionGlyphs();
}

SBase*
LayoutHandler::doListOfCompartmentGlyphs(const Attributes& a){
    return & currentLayout->getListOfCompartmentGlyphs();
}

SBase*
LayoutHandler::doListOfAdditionalGraphicalObjects(const Attributes& a){
    return & currentLayout->getListOfAdditionalGraphicalObjects();
}





SBase*
LayoutHandler::doSpeciesReferenceGlyph(const Attributes& a){
    SpeciesReferenceGlyph* srg=currentLayout->createSpeciesReferenceGlyph();
    std::string s;
    SpeciesReferenceRole_t role;

    
    XMLUtil::scanAttr(a, ATTR_ID, srg->id);
    if(XMLUtil::scanAttr(a, ATTR_SPECIES_REFERENCE_ID, s)==true){
        srg->setSpeciesReferenceId(s);
    }
    if(XMLUtil::scanAttr(a, ATTR_SPECIES_GLYPH, s)==true){
        srg->setSpeciesGlyphId(s);
    }
    if(XMLUtil::scanAttr(a, ATTR_ROLE, (unsigned int*)(&role))==true){
        srg->setRole(role);
    }
    return (SBase*)srg;
}



SBase*
LayoutHandler::doCurve(const Attributes& a){
    SBase* obj=static_cast<SBase*>(Stack_peek(fObjStack));
    ListOf* l=NULL;
    Curve* curve;

    if(obj->getTypeCode() == SBML_LAYOUT_REACTIONGLYPH)
    {
        ReactionGlyph* rg;
        rg=static_cast<ReactionGlyph*>(obj);
        curve=rg->getCurve();
    }
    if(obj->getTypeCode() == SBML_LAYOUT_SPECIESREFERENCEGLYPH)
    {
        SpeciesReferenceGlyph* srg; 
        srg=static_cast<SpeciesReferenceGlyph*>(obj);
        curve=srg->getCurve();
    }
    return (SBase*)curve;
}

SBase*
LayoutHandler::doListOfCurveSegments(const Attributes& a){
    SBase* obj=static_cast<SBase*>(Stack_peek(fObjStack));
    ListOf* l=NULL;
    Curve* curve;

    if(obj->getTypeCode() == SBML_LAYOUT_CURVE)
    {
        curve=static_cast<Curve*>(obj);
        l=&curve->getListOfCurveSegments();
    }
    return (SBase*)l;
}

SBase*
LayoutHandler::doListOfTextGlyphs(const Attributes& a){
    SBase* obj=static_cast<SBase*>(Stack_peek(fObjStack));
    ListOf* l=NULL;
    Layout* layout;

    if(obj->getTypeCode() == SBML_LAYOUT_LAYOUT)
    {
        layout=static_cast<Layout*>(obj);
        l=&layout->getListOfTextGlyphs();
    }
    return (SBase*)l;
}


SBase*
LayoutHandler::doCurveSegment(const Attributes& a){
    SBase* sb=NULL;
    std::string s;
    FILE* errStream;
    
    if (XMLUtil::scanAttr(a, ATTR_XSI_TYPE, s) == true)
    {
        if(strcmp(s.c_str(),"LineSegment")==0){
            sb=doLineSegment(a);
        }
	else if(strcmp(s.c_str(),"CubicBezier")==0){
            sb=doCubicBezier(a);
        }
        else{
          errStream=fdopen(2,"w");
          fprintf(errStream,"Error. Unknown CurveSegment %s.\n",s.c_str());
          fclose(errStream);
          exit(1);
        }        
    }
    return sb;
}

SBase*
LayoutHandler::doLineSegment(const Attributes& a){
    LineSegment* ls=currentLayout->createLineSegment();
    return ls;
}

SBase*
LayoutHandler::doCubicBezier(const Attributes& a){
    CubicBezier* cb=currentLayout->createCubicBezier();
    return cb;
}

SBase* LayoutHandler::doDimensions(const Attributes& a){
    SBase* obj=static_cast<SBase*>(Stack_peek(fObjStack));
    Dimensions* dimensions=NULL; 
    
    if(obj->getTypeCode()==SBML_LAYOUT_LAYOUT){
       Layout* layout=static_cast<Layout*>(obj);
       dimensions=&layout->getDimensions();
    }
    else if(obj->getTypeCode()==SBML_LAYOUT_BOUNDINGBOX){
        BoundingBox* box=static_cast<BoundingBox*>(obj);
        dimensions=&box->getDimensions();
    }
    if(dimensions){
        double width=0.0;
        double height=0.0;
        double depth=0.0;
     
        XMLUtil::scanAttr(a, ATTR_WIDTH, &width);
        XMLUtil::scanAttr(a,ATTR_HEIGHT, &height);
        if (XMLUtil::scanAttr(a, ATTR_DEPTH, &depth) != true)
        {
            depth=0.0;
        }
        dimensions->setBounds(width,height,depth);
    }
    return (SBase*)dimensions;
}

SBase* LayoutHandler::doPosition(const Attributes& a){
    SBase* obj=static_cast<SBase*>(Stack_peek(fObjStack));
    Point* point=NULL; 
    
    if(obj->getTypeCode()==SBML_LAYOUT_BOUNDINGBOX){
        BoundingBox* box=static_cast<BoundingBox*>(obj);
        point=&box->getPosition();
    }
    else{
        return NULL;
    }
    if(point){
        double x=0.0;
        double y=0.0;
        double z=0.0;
     
        XMLUtil::scanAttr(a, ATTR_X, &x);
        XMLUtil::scanAttr(a,ATTR_Y, &y);
        if (XMLUtil::scanAttr(a, ATTR_Z, &z) != true)
        {
            z=0.0;
        }
        point->setOffsets(x,y,z);
    }
    return (SBase*)point;
}

SBase* LayoutHandler::doStart(const Attributes& a){
    LineSegment* obj=dynamic_cast<LineSegment*>((SBase*)Stack_peek(fObjStack));
    Point* point=NULL; 
    
    if(obj){
        point=&obj->getStart();
    }
    if(point){
        double x=0.0;
        double y=0.0;
        double z=0.0;
     
        XMLUtil::scanAttr(a, ATTR_X, &x);
        XMLUtil::scanAttr(a,ATTR_Y, &y);
        if (XMLUtil::scanAttr(a, ATTR_Z, &z) != true)
        {
            z=0.0;
        }
        point->setOffsets(x,y,z);
    }
    return (SBase*)point;
}


SBase* LayoutHandler::doEnd(const Attributes& a){
    LineSegment* obj=dynamic_cast<LineSegment*>((SBase*)Stack_peek(fObjStack));
    Point* point=NULL; 
    
    if(obj){
        point=&obj->getEnd();
    }
    if(point){
        double x=0.0;
        double y=0.0;
        double z=0.0;
     
        XMLUtil::scanAttr(a, ATTR_X, &x);
        XMLUtil::scanAttr(a,ATTR_Y, &y);
        if (XMLUtil::scanAttr(a, ATTR_Z, &z) != true)
        {
            z=0.0;
        }
        point->setOffsets(x,y,z);
    }
    return (SBase*)point;
}


SBase* LayoutHandler::doBasePoint1(const Attributes& a){
    CubicBezier* obj=dynamic_cast<CubicBezier*>((SBase*)Stack_peek(fObjStack));
    Point* point=NULL; 
    
    if(obj){
        point=&obj->getBasePoint1();
    }
    if(point){
        double x=0.0;
        double y=0.0;
        double z=0.0;
     
        XMLUtil::scanAttr(a, ATTR_X, &x);
        XMLUtil::scanAttr(a,ATTR_Y, &y);
        if (XMLUtil::scanAttr(a, ATTR_Z, &z) != true)
        {
            z=0.0;
        }
        point->setOffsets(x,y,z);
    }
    return (SBase*)point;
}


SBase* LayoutHandler::doBasePoint2(const Attributes& a){
    CubicBezier* obj=dynamic_cast<CubicBezier*>((SBase*)Stack_peek(fObjStack));
    Point* point=NULL; 
    
    if(obj){
        point=&obj->getBasePoint2();
    }
    if(point){
        double x=0.0;
        double y=0.0;
        double z=0.0;
     
        XMLUtil::scanAttr(a, ATTR_X, &x);
        XMLUtil::scanAttr(a,ATTR_Y, &y);
        if (XMLUtil::scanAttr(a, ATTR_Z, &z) != true)
        {
            z=0.0;
        }
        point->setOffsets(x,y,z);
    }
    return (SBase*)point;
}


SBase* LayoutHandler::doBoundingBox(const Attributes& a){
    SBase* obj=static_cast<SBase*>(Stack_peek(fObjStack));
    GraphicalObject* go;
    BoundingBox* box=NULL;
    std::string id;
    
    if(go=dynamic_cast<GraphicalObject*>(obj)){
        box=&go->getBoundingBox();
    }
    if(box){
        if (XMLUtil::scanAttr(a, ATTR_ID, id) == true)
        {
            box->setId(id);
        }
    }
    return (SBase*)box;
}  

    
ParseMessage*
LayoutHandler::ParseMessage_createFrom (const char* message)
{
  return new
#ifdef USE_EXPAT
    ParseMessage( 100,message, 
                  getCurrentLineNumber(),
                  getCurrentColumnNumber() );
#else
    ParseMessage( 100, message, 
                 (unsigned int) fLocator->getLineNumber(),
                 (unsigned int) fLocator->getColumnNumber() );
#endif  // USE_EXPAT
}


#ifndef USE_EXPAT
/**
 * Creates a new ParseMessage from the given SAXException and returns a
 * pointer to it.
 *
 * The exception's message will be the text of the ParseMessage.  The line
 * and column number where the error occurred are obtained from this
 * handler's document Locator and are also stored in the ParseMessage.
 */
ParseMessage_t*
LayoutHandler::ParseMessage_createFrom (const SAXParseException& e)
{
  char*           message;
  ParseMessage_t* pm;


  message = XMLString::transcode( e.getMessage() );

  pm = new ParseMessage( 100, message, 
                         (unsigned int) e.getLineNumber(),
                         (unsigned int) e.getColumnNumber() );

  XMLString::release(&message);

  return pm;
}
#endif  // !USE_EXPAT

ListOf*
LayoutHandler::getListOfLayouts(){
    return this->listOfLayouts;
}

/**
 * Sets the line and column number of the given SBase object to the current
 * position in the SBML document.  If the line and column number of the
 * document are not available, this method does nothing.
 */
void
LayoutHandler::setLineAndColumn (SBase* sb)
{
  int line   = 0;
  int column = 0;


#ifdef USE_EXPAT

  line   = getCurrentLineNumber  ();
  column = getCurrentColumnNumber();

#else

  if (fLocator != 0)
  {
    line   = fLocator->getLineNumber  ();
    column = fLocator->getColumnNumber();
  }

#endif  // USE_EXPAT

  if (line > 0)
  {
    sb->line = line;
  }

  if (column > 0)
  {
    sb->column = column;
  }
}
