/**
 * Filename    : TestLayoutHandler.cpp
 * Description : Unit tests for LayoutHandler
 * Organization: European Media Laboratories Research gGmbH
 * Created     : 2005-05-03
 *
 * Copyright 2005 European Media Laboratories Research gGmbH
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
#include <check.h>

#include <common/common.h>
#include <common/extern.h>

#include "LayoutHandler.h"
#include "sbml/ListOf.h"

BEGIN_C_DECLS

#define XML_HEADER    "<?xml version='1.0' encoding='UTF-8'?>\n"
#define SBML_HEADER2  //"<sbml level='2' version='1'> <model name='testModel'> <annotation xmlns=\"http://projects.eml.org/bcb/sbml/level2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
#define SBML_FOOTER   //"</annotation></model> </sbml>"

#define NOTES       "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n <p>Testnote</p>\n</body>"

/**
 * Wraps the string s in the appropriate XML or SBML boilerplate.
 */
#define wrapSBML2(s)  XML_HEADER SBML_HEADER2 s SBML_FOOTER


static LayoutHandler         *LH;  
static ListOf                *LISTOFLAYOUTS;

void
LayoutHandlerTest_setup (void)
{
    LISTOFLAYOUTS=new ListOf();
    LH=new LayoutHandler(LISTOFLAYOUTS);
    LH->startDocument();
    LH->enableElementHandler();
}

void 
LayoutHandlerTest_teardown (void)
{
    LH->endDocument();
    delete LH;
    delete LISTOFLAYOUTS;
}

START_TEST (test_LayoutHandler_Layout)
{
    const char* s = wrapSBML2
    (
      "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfCompartmentGlyphs>\n"
      "    <compartmentGlyph id=\"compartmentGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"0\" y=\"0\"/>\n"
      "        <dimensions width=\"0\" height=\"0\"/>\n" 
      "      </boundingBox>\n"  
      "    </compartmentGlyph>\n"
      "  </listOfCompartmentGlyphs>\n"
      "  <listOfSpeciesGlyphs>\n"
      "    <speciesGlyph id=\"speciesGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"0\" y=\"0\"/>\n"
      "        <dimensions width=\"0\" height=\"0\"/>\n" 
      "      </boundingBox>\n"  
      "    </speciesGlyph>\n"
      "  </listOfSpeciesGlyphs>\n"
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"0\" y=\"0\"/>\n"
      "        <dimensions width=\"0\" height=\"0\"/>\n" 
      "      </boundingBox>\n"  
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "  <listOfTextGlyphs>\n"
      "    <textGlyph id=\"textGlyph_1\" text=\"test\">\n"
      "      <boundingBox>\n"
      "        <position x=\"0\" y=\"0\"/>\n"
      "        <dimensions width=\"0\" height=\"0\"/>\n" 
      "      </boundingBox>\n"  
      "    </textGlyph>\n"
      "  </listOfTextGlyphs>\n"
      "  <listOfAdditionalGraphicalObjects>\n"
      "    <graphicalObject id=\"graphicalObject_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"0\" y=\"0\"/>\n"
      "        <dimensions width=\"0\" height=\"0\"/>\n" 
      "      </boundingBox>\n"  
      "    </graphicalObject>\n"
      "  </listOfAdditionalGraphicalObjects>\n"
      "</layout>\n"
    );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getNumCompartmentGlyphs()==1);
    CompartmentGlyph* cg=l->getCompartmentGlyph(0);
    fail_unless(cg!=NULL);
    fail_unless(cg->getId()=="compartmentGlyph_1");
    BoundingBox* box=&cg->getBoundingBox();
    fail_unless(!box->isSetId());
    Point* position=&box->getPosition();
    fail_unless(position->getXOffset()==0.0);
    fail_unless(position->getYOffset()==0.0);
    fail_unless(position->getZOffset()==0.0);
    dimensions=&box->getDimensions();
    fail_unless(dimensions->getWidth()==0.0);
    fail_unless(dimensions->getHeight()==0.0);
    fail_unless(dimensions->getDepth()==0.0);
    
    fail_unless(l->getNumSpeciesGlyphs()==1);
    SpeciesGlyph* sg=l->getSpeciesGlyph(0);
    fail_unless(sg!=NULL);
    fail_unless(sg->getId()=="speciesGlyph_1");
    box=&sg->getBoundingBox();
    fail_unless(!box->isSetId());
    position=&box->getPosition();
    fail_unless(position->getXOffset()==0.0);
    fail_unless(position->getYOffset()==0.0);
    fail_unless(position->getZOffset()==0.0);
    dimensions=&box->getDimensions();
    fail_unless(dimensions->getWidth()==0.0);
    fail_unless(dimensions->getHeight()==0.0);
    fail_unless(dimensions->getDepth()==0.0);

    
    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg!=NULL);
    fail_unless(rg->getId()=="reactionGlyph_1");
    box=&rg->getBoundingBox();
    fail_unless(!box->isSetId());
    position=&box->getPosition();
    fail_unless(position->getXOffset()==0.0);
    fail_unless(position->getYOffset()==0.0);
    fail_unless(position->getZOffset()==0.0);
    dimensions=&box->getDimensions();
    fail_unless(dimensions->getWidth()==0.0);
    fail_unless(dimensions->getHeight()==0.0);
    fail_unless(dimensions->getDepth()==0.0);

    
    fail_unless(l->getNumTextGlyphs()==1);
    TextGlyph* tg=l->getTextGlyph(0);
    fail_unless(tg!=NULL);
    fail_unless(tg->getId()=="textGlyph_1");
    box=&tg->getBoundingBox();
    fail_unless(!box->isSetId());
    position=&box->getPosition();
    fail_unless(position->getXOffset()==0.0);
    fail_unless(position->getYOffset()==0.0);
    fail_unless(position->getZOffset()==0.0);
    dimensions=&box->getDimensions();
    fail_unless(dimensions->getWidth()==0.0);
    fail_unless(dimensions->getHeight()==0.0);
    fail_unless(dimensions->getDepth()==0.0);

    
    fail_unless(l->getNumAdditionalGraphicalObjects()==1);
    GraphicalObject* go=l->getAdditionalGraphicalObject(0);
    fail_unless(go!=NULL);
    fail_unless(go->getId()=="graphicalObject_1");
    box=&go->getBoundingBox();
    fail_unless(!box->isSetId());
    position=&box->getPosition();
    fail_unless(position->getXOffset()==0.0);
    fail_unless(position->getYOffset()==0.0);
    fail_unless(position->getZOffset()==0.0);
    dimensions=&box->getDimensions();
    fail_unless(dimensions->getWidth()==0.0);
    fail_unless(dimensions->getHeight()==0.0);
    fail_unless(dimensions->getDepth()==0.0);
}
END_TEST

START_TEST (test_LayoutHandler_Layout_notes)
{
    
    
    const char* s = wrapSBML2
    (
      "<layout id=\"layout_1\">\n"
      "  <notes>"   
           "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
           " <p>Testnote</p>\n"
           "</body>"
        "</notes>\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "</layout>\n"     
    );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getNotes()==NOTES);

}
END_TEST

START_TEST (test_LayoutHandler_Layout_annotation)
{
    const char* s = wrapSBML2
    (
      "<layout id=\"layout_1\">\n"
      "  <annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "</layout>\n"     
    );

    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

}
END_TEST

START_TEST (test_LayoutHandler_Layout_skipOptional)
{
    const char* s = wrapSBML2
    (
      "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "</layout>\n"     
    );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getNumCompartmentGlyphs()==0);
    fail_unless(l->getNumSpeciesGlyphs()==0);
    fail_unless(l->getNumReactionGlyphs()==0);
    fail_unless(l->getNumTextGlyphs()==0);
    fail_unless(l->getNumAdditionalGraphicalObjects()==0);

}
END_TEST


START_TEST (test_LayoutHandler_CompartmentGlyph)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfCompartmentGlyphs>\n"
      "    <compartmentGlyph id=\"compartmentGlyph_1\" compartment=\"compartment_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </compartmentGlyph>\n"
      "  </listOfCompartmentGlyphs>\n"
      "</layout>\n"
    );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);
    
    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumCompartmentGlyphs()==1);
    CompartmentGlyph* cg=l->getCompartmentGlyph(0);
    fail_unless(cg->getId()=="compartmentGlyph_1");
    fail_unless(cg->getCompartmentId()=="compartment_1");

    const BoundingBox& bb=cg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 

}
END_TEST

START_TEST (test_LayoutHandler_CompartmentGlyph_notes)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfCompartmentGlyphs>\n"
      "    <compartmentGlyph id=\"compartmentGlyph_1\">\n"
      "      <notes>"   
              "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
              " <p>Testnote</p>\n"
              "</body>"
             "</notes>\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </compartmentGlyph>\n"
      "  </listOfCompartmentGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumCompartmentGlyphs()==1);
    CompartmentGlyph* cg=l->getCompartmentGlyph(0);
    fail_unless(cg->getId()=="compartmentGlyph_1");
    fail_unless(!cg->isSetCompartmentId());

    const BoundingBox& bb=cg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 

    fail_unless(cg->getNotes()==NOTES);
    
}
END_TEST

START_TEST (test_LayoutHandler_CompartmentGlyph_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfCompartmentGlyphs>\n"
      "    <compartmentGlyph id=\"compartmentGlyph_1\">\n"
      "      <annotation>\n"
      "        <this-is-a-test/>\n"
      "      </annotation>\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </compartmentGlyph>\n"
      "  </listOfCompartmentGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumCompartmentGlyphs()==1);
    CompartmentGlyph* cg=l->getCompartmentGlyph(0);
    fail_unless(cg->getId()=="compartmentGlyph_1");
    fail_unless(!cg->isSetCompartmentId());

    const BoundingBox& bb=cg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
}
END_TEST

START_TEST (test_LayoutHandler_CompartmentGlyph_skipOptional)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfCompartmentGlyphs>\n"
      "    <compartmentGlyph id=\"compartmentGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </compartmentGlyph>\n"
      "  </listOfCompartmentGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumCompartmentGlyphs()==1);
    CompartmentGlyph* cg=l->getCompartmentGlyph(0);
    fail_unless(cg->getId()=="compartmentGlyph_1");
    fail_unless(!cg->isSetCompartmentId());

    const BoundingBox& bb=cg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
}
END_TEST

START_TEST (test_LayoutHandler_SpeciesGlyph)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfSpeciesGlyphs>\n"
      "    <speciesGlyph id=\"speciesGlyph_1\" species=\"species_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </speciesGlyph>\n"
      "  </listOfSpeciesGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumSpeciesGlyphs()==1);
    SpeciesGlyph* sg=l->getSpeciesGlyph(0);
    fail_unless(sg->getId()=="speciesGlyph_1");
    fail_unless(sg->getSpeciesId()=="species_1");

    const BoundingBox& bb=sg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
}
END_TEST

START_TEST (test_LayoutHandler_SpeciesGlyph_notes)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfSpeciesGlyphs>\n"
      "    <speciesGlyph id=\"speciesGlyph_1\">\n"
          "  <notes>"   
              "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
              " <p>Testnote</p>\n"
              "</body>"
            "</notes>\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </speciesGlyph>\n"
      "  </listOfSpeciesGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumSpeciesGlyphs()==1);
    SpeciesGlyph* sg=l->getSpeciesGlyph(0);
    fail_unless(sg->getId()=="speciesGlyph_1");
    fail_unless(!sg->isSetSpeciesId());

    const BoundingBox& bb=sg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
    fail_unless(sg->getNotes()==NOTES);
        
}
END_TEST

START_TEST (test_LayoutHandler_SpeciesGlyph_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfSpeciesGlyphs>\n"
      "    <speciesGlyph id=\"speciesGlyph_1\">\n"
      "      <annotation>\n"
      "        <this-is-a-test/>\n"
      "      </annotation>\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </speciesGlyph>\n"
      "  </listOfSpeciesGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumSpeciesGlyphs()==1);
    SpeciesGlyph* sg=l->getSpeciesGlyph(0);
    fail_unless(sg->getId()=="speciesGlyph_1");
    fail_unless(!sg->isSetSpeciesId());

    const BoundingBox& bb=sg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 

    
}
END_TEST

START_TEST (test_LayoutHandler_SpeciesGlyph_skipOptional)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfSpeciesGlyphs>\n"
      "    <speciesGlyph id=\"speciesGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </speciesGlyph>\n"
      "  </listOfSpeciesGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumSpeciesGlyphs()==1);
    SpeciesGlyph* sg=l->getSpeciesGlyph(0);
    fail_unless(sg->getId()=="speciesGlyph_1");
    
    fail_unless(!sg->isSetSpeciesId());

    const BoundingBox& bb=sg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
}
END_TEST

START_TEST (test_LayoutHandler_ReactionGlyph_Curve)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\" reaction=\"reaction_1\">\n"
      "      <curve>\n"
      "        <listOfCurveSegments>\n"
      "          <curveSegment xsi:type=\"LineSegment\">\n" 
      "            <start x=\"10\" y=\"15\"/>\n" 
      "            <end x=\"20\" y=\"30\"/>\n" 
      "          </curveSegment>\n"
      "        </listOfCurveSegments>\n"
      "      </curve>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(rg->getReactionId()=="reaction_1");

    fail_unless(rg->isSetCurve());
    const Curve* curve=rg->getCurve();
    fail_unless(curve->getNumCurveSegments()==1);
    const LineSegment* ls=curve->getCurveSegment(0);
    fail_unless(dynamic_cast<const CubicBezier*>(ls)==NULL);
    const Point& start=ls->getStart(); 
    fail_unless(start.getXOffset()==10.0);
    fail_unless(start.getYOffset()==15.0);
    fail_unless(start.getZOffset()==0.0);
    const Point& end=ls->getEnd(); 
    fail_unless(end.getXOffset()==20.0);
    fail_unless(end.getYOffset()==30.0);
    fail_unless(end.getZOffset()==0.0);

    
}
END_TEST

START_TEST (test_LayoutHandler_ReactionGlyph_BoundingBox)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\" reaction=\"reaction_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(rg->getReactionId()=="reaction_1");

    const BoundingBox& bb=rg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
}
END_TEST

START_TEST (test_LayoutHandler_ReactionGlyph_notes)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\" reaction=\"reaction_1\">\n"
      "      <notes>"   
              "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
              " <p>Testnote</p>\n"
              "</body>"
            "</notes>\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(rg->getReactionId()=="reaction_1");

    const BoundingBox& bb=rg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 

    fail_unless(rg->getNotes()==NOTES);
    
}
END_TEST

START_TEST (test_LayoutHandler_ReactionGlyph_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\" reaction=\"reaction_1\">\n"
      "      <annotation>\n"
      "        <this-is-a-test/>\n"
      "      </annotation>\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(rg->getReactionId()=="reaction_1");

    const BoundingBox& bb=rg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
}
END_TEST

START_TEST (test_LayoutHandler_ReactionGlyph_skipOptional)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    const BoundingBox& bb=rg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
}
END_TEST

START_TEST (test_LayoutHandler_SpeciesReferenceGlyph_Curve)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"
      "      <listOfSpeciesReferenceGlyphs>\n"
      "        <speciesReferenceGlyph id=\"speciesReferenceGlyph_1\" speciesReference=\"speciesReference_1\" speciesGlyph=\"speciesGlyph_1\" role=\"activator\">\n"
      "          <curve>\n"
      "            <listOfCurveSegments>\n"
      "              <curveSegment xsi:type=\"LineSegment\">\n" 
      "                <start x=\"10\" y=\"15\"/>\n" 
      "                <end x=\"20\" y=\"30\"/>\n" 
      "              </curveSegment>\n"
      "            </listOfCurveSegments>\n"
      "          </curve>\n"
      "        </speciesReferenceGlyph>\n"
      "      </listOfSpeciesReferenceGlyphs>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
    );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    const BoundingBox& bb=rg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
    fail_unless(rg->getNumSpeciesReferenceGlyphs()==1);
    SpeciesReferenceGlyph* srg=rg->getSpeciesReferenceGlyph(0);
    fail_unless(srg->getId()=="speciesReferenceGlyph_1");
    fail_unless(srg->getRole()==SPECIES_ROLE_ACTIVATOR);
    fail_unless(srg->getSpeciesGlyphId()=="speciesGlyph_1");
    fail_unless(srg->getSpeciesReferenceId()=="speciesReference_1");

    fail_unless(srg->isSetCurve());
    const Curve* curve=srg->getCurve();
    fail_unless(curve->getNumCurveSegments()==1);
    const LineSegment* ls=curve->getCurveSegment(0);
    fail_unless(dynamic_cast<const CubicBezier*>(ls)==NULL);
    const Point& start=ls->getStart(); 
    fail_unless(start.getXOffset()==10.0);
    fail_unless(start.getYOffset()==15.0);
    fail_unless(start.getZOffset()==0.0);
    const Point& end=ls->getEnd(); 
    fail_unless(end.getXOffset()==20.0);
    fail_unless(end.getYOffset()==30.0);
    fail_unless(end.getZOffset()==0.0);
    
}
END_TEST

START_TEST (test_LayoutHandler_SpeciesReferenceGlyph_BoundingBox)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"
      "      <listOfSpeciesReferenceGlyphs>\n"
      "        <speciesReferenceGlyph id=\"speciesReferenceGlyph_1\" speciesReference=\"speciesReference_1\" speciesGlyph=\"speciesGlyph_1\" role=\"modifier\">\n"
      "          <boundingBox>\n"
      "            <position x=\"110.3\" y=\"120\"/>\n"
      "            <dimensions width=\"20.5\" height=\"40.5\"/>\n" 
      "          </boundingBox>\n"  
      "        </speciesReferenceGlyph>\n"
      "      </listOfSpeciesReferenceGlyphs>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    const BoundingBox& bb=rg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions3=bb.getDimensions();
    fail_unless(dimensions3.getWidth()==200.5); 
    fail_unless(dimensions3.getHeight()==400.5); 
    fail_unless(dimensions3.getDepth()==0.0); 
    
    fail_unless(rg->getNumSpeciesReferenceGlyphs()==1);
    SpeciesReferenceGlyph* srg=rg->getSpeciesReferenceGlyph(0);
    fail_unless(srg->getId()=="speciesReferenceGlyph_1");
    fail_unless(srg->getRole()==SPECIES_ROLE_MODIFIER);
    fail_unless(srg->getSpeciesGlyphId()=="speciesGlyph_1");
    fail_unless(srg->getSpeciesReferenceId()=="speciesReference_1");

    const BoundingBox& bb2=srg->getBoundingBox();
    const Point& position2=bb2.getPosition();
    fail_unless(position2.getXOffset()==110.3);
    fail_unless(position2.getYOffset()==120.0);
    fail_unless(position2.getZOffset()==0.0);
    const Dimensions& dimensions2=bb2.getDimensions();
    fail_unless(dimensions2.getWidth()==20.5); 
    fail_unless(dimensions2.getHeight()==40.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
}
END_TEST

START_TEST (test_LayoutHandler_SpeciesReferenceGlyph_notes)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"
      "      <listOfSpeciesReferenceGlyphs>\n"
      "        <speciesReferenceGlyph id=\"speciesReferenceGlyph_1\" role=\"substrate\">\n"
      "          <notes>"   
                  "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
                  " <p>Testnote</p>\n"
                  "</body>"
                "</notes>\n"
      "          <boundingBox>\n"
      "            <position x=\"110.3\" y=\"120\"/>\n"
      "            <dimensions width=\"20.5\" height=\"40.5\"/>\n" 
      "          </boundingBox>\n"  
      "        </speciesReferenceGlyph>\n"
      "      </listOfSpeciesReferenceGlyphs>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    const BoundingBox& bb=rg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions3=bb.getDimensions();
    fail_unless(dimensions3.getWidth()==200.5); 
    fail_unless(dimensions3.getHeight()==400.5); 
    fail_unless(dimensions3.getDepth()==0.0); 
    
    fail_unless(rg->getNumSpeciesReferenceGlyphs()==1);
    SpeciesReferenceGlyph* srg=rg->getSpeciesReferenceGlyph(0);
    fail_unless(srg->getId()=="speciesReferenceGlyph_1");
    fail_unless(srg->getRole()==SPECIES_ROLE_SUBSTRATE);
    fail_unless(!srg->isSetSpeciesGlyphId());
    fail_unless(!srg->isSetSpeciesReferenceId());

    const BoundingBox& bb2=srg->getBoundingBox();
    const Point& position2=bb2.getPosition();
    fail_unless(position2.getXOffset()==110.3);
    fail_unless(position2.getYOffset()==120.0);
    fail_unless(position2.getZOffset()==0.0);
    const Dimensions& dimensions2=bb2.getDimensions();
    fail_unless(dimensions2.getWidth()==20.5); 
    fail_unless(dimensions2.getHeight()==40.5); 
    fail_unless(dimensions2.getDepth()==0.0); 

    fail_unless(srg->getNotes()==NOTES);
}
END_TEST

START_TEST (test_LayoutHandler_SpeciesReferenceGlyph_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"
      "      <listOfSpeciesReferenceGlyphs>\n"
      "        <speciesReferenceGlyph id=\"speciesReferenceGlyph_1\" role=\"sideproduct\">\n"
      "          <annotation>\n"
      "            <this-is-a-test/>\n"
      "          </annotation>\n"
      "          <boundingBox>\n"
      "            <position x=\"110.3\" y=\"120\"/>\n"
      "            <dimensions width=\"20.5\" height=\"40.5\"/>\n" 
      "          </boundingBox>\n"  
      "        </speciesReferenceGlyph>\n"
      "      </listOfSpeciesReferenceGlyphs>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    const BoundingBox& bb=rg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions3=bb.getDimensions();
    fail_unless(dimensions3.getWidth()==200.5); 
    fail_unless(dimensions3.getHeight()==400.5); 
    fail_unless(dimensions3.getDepth()==0.0); 
    
    fail_unless(rg->getNumSpeciesReferenceGlyphs()==1);
    SpeciesReferenceGlyph* srg=rg->getSpeciesReferenceGlyph(0);
    fail_unless(srg->getId()=="speciesReferenceGlyph_1");
    fail_unless(srg->getRole()==SPECIES_ROLE_SIDEPRODUCT);
    fail_unless(!srg->isSetSpeciesGlyphId());
    fail_unless(!srg->isSetSpeciesReferenceId());

    const BoundingBox& bb2=srg->getBoundingBox();
    const Point& position2=bb2.getPosition();
    fail_unless(position2.getXOffset()==110.3);
    fail_unless(position2.getYOffset()==120.0);
    fail_unless(position2.getZOffset()==0.0);
    const Dimensions& dimensions2=bb2.getDimensions();
    fail_unless(dimensions2.getWidth()==20.5); 
    fail_unless(dimensions2.getHeight()==40.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
}
END_TEST

START_TEST (test_LayoutHandler_SpeciesReferenceGlyph_skipOptional)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"
      "      <listOfSpeciesReferenceGlyphs>\n"
      "        <speciesReferenceGlyph id=\"speciesReferenceGlyph_1\" role=\"sidesubstrate\">\n"
      "          <boundingBox>\n"
      "            <position x=\"110.3\" y=\"120\"/>\n"
      "            <dimensions width=\"20.5\" height=\"40.5\"/>\n" 
      "          </boundingBox>\n"  
      "        </speciesReferenceGlyph>\n"
      "      </listOfSpeciesReferenceGlyphs>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    const BoundingBox& bb=rg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions3=bb.getDimensions();
    fail_unless(dimensions3.getWidth()==200.5); 
    fail_unless(dimensions3.getHeight()==400.5); 
    fail_unless(dimensions3.getDepth()==0.0); 
    
    fail_unless(rg->getNumSpeciesReferenceGlyphs()==1);
    SpeciesReferenceGlyph* srg=rg->getSpeciesReferenceGlyph(0);
    fail_unless(srg->getId()=="speciesReferenceGlyph_1");
    fail_unless(srg->getRole()==SPECIES_ROLE_SIDESUBSTRATE);
    fail_unless(!srg->isSetSpeciesGlyphId());
    fail_unless(!srg->isSetSpeciesReferenceId());

    const BoundingBox& bb2=srg->getBoundingBox();
    const Point& position2=bb2.getPosition();
    fail_unless(position2.getXOffset()==110.3);
    fail_unless(position2.getYOffset()==120.0);
    fail_unless(position2.getZOffset()==0.0);
    const Dimensions& dimensions2=bb2.getDimensions();
    fail_unless(dimensions2.getWidth()==20.5); 
    fail_unless(dimensions2.getHeight()==40.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
 }
END_TEST

START_TEST (test_LayoutHandler_TextGlyph_text)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfTextGlyphs>\n"
      "    <textGlyph id=\"textGlyph_1\" graphicalObject=\"speciesGlyph_1\" text=\"test text\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </textGlyph>\n"
      "  </listOfTextGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumTextGlyphs()==1);
    TextGlyph* tg=l->getTextGlyph(0);
    fail_unless(tg->getId()=="textGlyph_1");
    
    fail_unless(tg->getGraphicalObjectId()=="speciesGlyph_1");
    fail_unless(tg->getText()=="test text");

    const BoundingBox& bb=tg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
}
END_TEST

START_TEST (test_LayoutHandler_TextGlyph_originOfText)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfTextGlyphs>\n"
      "    <textGlyph id=\"textGlyph_1\" graphicalObject=\"speciesGlyph_1\" originOfText=\"reactionGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </textGlyph>\n"
      "  </listOfTextGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumTextGlyphs()==1);
    TextGlyph* tg=l->getTextGlyph(0);
    fail_unless(tg->getId()=="textGlyph_1");
    
    fail_unless(tg->getGraphicalObjectId()=="speciesGlyph_1");
    fail_unless(tg->getOriginOfTextId()=="reactionGlyph_1");

    const BoundingBox& bb=tg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
}
END_TEST

START_TEST (test_LayoutHandler_TextGlyph_notes)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfTextGlyphs>\n"
      "    <textGlyph id=\"textGlyph_1\" graphicalObject=\"speciesGlyph_1\" originOfText=\"reactionGlyph_1\">\n"
      "      <notes>"   
              "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
              " <p>Testnote</p>\n"
              "</body>"
            "</notes>\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </textGlyph>\n"
      "  </listOfTextGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumTextGlyphs()==1);
    TextGlyph* tg=l->getTextGlyph(0);
    fail_unless(tg->getId()=="textGlyph_1");
    
    fail_unless(tg->getGraphicalObjectId()=="speciesGlyph_1");
    fail_unless(tg->getOriginOfTextId()=="reactionGlyph_1");

    const BoundingBox& bb=tg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 

    fail_unless(tg->getNotes()==NOTES);
    
}
END_TEST

START_TEST (test_LayoutHandler_TextGlyph_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfTextGlyphs>\n"
      "    <textGlyph id=\"textGlyph_1\" graphicalObject=\"speciesGlyph_1\" originOfText=\"reactionGlyph_1\">\n"
      "      <annotation>\n"
      "        <this-is-a-test/>\n"
      "      </annotation>\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </textGlyph>\n"
      "  </listOfTextGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumTextGlyphs()==1);
    TextGlyph* tg=l->getTextGlyph(0);
    fail_unless(tg->getId()=="textGlyph_1");
    
    fail_unless(tg->getGraphicalObjectId()=="speciesGlyph_1");
    fail_unless(tg->getOriginOfTextId()=="reactionGlyph_1");

    const BoundingBox& bb=tg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
}
END_TEST

START_TEST (test_LayoutHandler_TextGlyph_skipOptional)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfTextGlyphs>\n"
      "    <textGlyph id=\"textGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </textGlyph>\n"
      " </listOfTextGlyphs>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumTextGlyphs()==1);
    TextGlyph* tg=l->getTextGlyph(0);
    fail_unless(tg->getId()=="textGlyph_1");
    
    fail_unless(!tg->isSetGraphicalObjectId());
    fail_unless(!tg->isSetOriginOfTextId());
    fail_unless(!tg->isSetText());

    const BoundingBox& bb=tg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
}
END_TEST

START_TEST (test_LayoutHandler_GraphicalObject)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfAdditionalGraphicalObjects>\n"
      "    <graphicalObject id=\"graphicalObject_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </graphicalObject>\n"
      "  </listOfAdditionalGraphicalObjects>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");
    
    fail_unless(l->getNumAdditionalGraphicalObjects()==1);
    GraphicalObject* go=l->getAdditionalGraphicalObject(0);
    fail_unless(go->getId()=="graphicalObject_1");
    const BoundingBox& bb=go->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
}
END_TEST

START_TEST (test_LayoutHandler_GraphicalObject_notes)
{
    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfAdditionalGraphicalObjects>\n"
      "    <graphicalObject id=\"graphicalObject_1\">\n"
      "      <notes>"   
              "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
              " <p>Testnote</p>\n"
              "</body>"
            "</notes>\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </graphicalObject>\n"
      "  </listOfAdditionalGraphicalObjects>\n"
      "</layout>\n"
   );
    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");
    
    fail_unless(l->getNumAdditionalGraphicalObjects()==1);
    GraphicalObject* go=l->getAdditionalGraphicalObject(0);
    fail_unless(go->getId()=="graphicalObject_1");
    const BoundingBox& bb=go->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 

    fail_unless(go->getNotes()==NOTES);
    
}
END_TEST

START_TEST (test_LayoutHandler_GraphicalObject_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapSBML2
    ( "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfAdditionalGraphicalObjects>\n"
      "    <graphicalObject id=\"graphicalObject_1\">\n"
      "      <annotation>\n"
      "        <this-is-a-test/>\n"
      "      </annotation>\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </graphicalObject>\n"
      "  </listOfAdditionalGraphicalObjects>\n"
      "</layout>\n"
   );

    
    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");
    
    fail_unless(l->getNumAdditionalGraphicalObjects()==1);
    GraphicalObject* go=l->getAdditionalGraphicalObject(0);
    fail_unless(go->getId()=="graphicalObject_1");
    const BoundingBox& bb=go->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 
    
    
}
END_TEST

START_TEST (test_LayoutHandler_Curve)
{
    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <curve>\n"
      "        <listOfCurveSegments>\n"
      "          <curveSegment xsi:type=\"LineSegment\">\n" 
      "            <start x=\"10\" y=\"15\"/>\n" 
      "            <end x=\"20\" y=\"30\"/>\n" 
      "          </curveSegment>\n"
      "        </listOfCurveSegments>\n"
      "      </curve>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    fail_unless(rg->isSetCurve());
    const Curve* curve=rg->getCurve();
    fail_unless(curve->getNumCurveSegments()==1);
    const LineSegment* ls=curve->getCurveSegment(0);
    fail_unless(dynamic_cast<const CubicBezier*>(ls)==NULL);
    const Point& start=ls->getStart(); 
    fail_unless(start.getXOffset()==10.0);
    fail_unless(start.getYOffset()==15.0);
    fail_unless(start.getZOffset()==0.0);
    const Point& end=ls->getEnd(); 
    fail_unless(end.getXOffset()==20.0);
    fail_unless(end.getYOffset()==30.0);
    fail_unless(end.getZOffset()==0.0);

}
END_TEST

START_TEST (test_LayoutHandler_Curve_notes)
{
    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <curve>\n"
      "        <notes>"   
                "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
                " <p>Testnote</p>\n"
                "</body>"
              "</notes>\n"
      "        <listOfCurveSegments>\n"
      "          <curveSegment xsi:type=\"LineSegment\">\n" 
      "            <start x=\"10\" y=\"15\"/>\n" 
      "            <end x=\"20\" y=\"30\"/>\n" 
      "          </curveSegment>\n"
      "        </listOfCurveSegments>\n"
      "      </curve>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    fail_unless(rg->isSetCurve());
    const Curve* curve=rg->getCurve();
    fail_unless(curve->getNumCurveSegments()==1);
    const LineSegment* ls=curve->getCurveSegment(0);
    fail_unless(dynamic_cast<const CubicBezier*>(ls)==NULL);
    const Point& start=ls->getStart(); 
    fail_unless(start.getXOffset()==10.0);
    fail_unless(start.getYOffset()==15.0);
    fail_unless(start.getZOffset()==0.0);
    const Point& end=ls->getEnd(); 
    fail_unless(end.getXOffset()==20.0);
    fail_unless(end.getYOffset()==30.0);
    fail_unless(end.getZOffset()==0.0);

    fail_unless(curve->getNotes()==NOTES);

}
END_TEST

START_TEST (test_LayoutHandler_Curve_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <curve>\n"
      "        <annotation>\n"
      "          <this-is-a-test/>\n"
      "        </annotation>\n"
      "        <listOfCurveSegments>\n"
      "          <curveSegment xsi:type=\"LineSegment\">\n" 
      "            <start x=\"10\" y=\"15\"/>\n" 
      "            <end x=\"20\" y=\"30\"/>\n" 
      "          </curveSegment>\n"
      "        </listOfCurveSegments>\n"
      "      </curve>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
    );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    fail_unless(rg->isSetCurve());
    const Curve* curve=rg->getCurve();
    fail_unless(curve->getNumCurveSegments()==1);
    const LineSegment* ls=curve->getCurveSegment(0);
    fail_unless(dynamic_cast<const CubicBezier*>(ls)==NULL);
    const Point& start=ls->getStart(); 
    fail_unless(start.getXOffset()==10.0);
    fail_unless(start.getYOffset()==15.0);
    const Point& end=ls->getEnd(); 
    fail_unless(end.getXOffset()==20.0);
    fail_unless(end.getYOffset()==30.0);
}
END_TEST

START_TEST (test_LayoutHandler_Curve_skipOptional)
{
    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <curve>\n"
      "      </curve>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    fail_unless(!rg->isSetCurve());
}
END_TEST

START_TEST (test_LayoutHandler_LineSegment)
{
    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <curve>\n"
      "        <listOfCurveSegments>\n"
      "          <curveSegment xsi:type=\"LineSegment\">\n" 
      "            <start x=\"10\" y=\"15\"/>\n" 
      "            <end x=\"20\" y=\"30\"/>\n" 
      "          </curveSegment>\n"
      "        </listOfCurveSegments>\n"
      "      </curve>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
  );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    fail_unless(rg->isSetCurve());
    const Curve* curve=rg->getCurve();
    fail_unless(curve->getNumCurveSegments()==1);
    const LineSegment* ls=curve->getCurveSegment(0);
    fail_unless(dynamic_cast<const CubicBezier*>(ls)==NULL);
    const Point& start=ls->getStart(); 
    fail_unless(start.getXOffset()==10.0);
    fail_unless(start.getYOffset()==15.0);
    const Point& end=ls->getEnd(); 
    fail_unless(end.getXOffset()==20.0);
    fail_unless(end.getYOffset()==30.0);
}
END_TEST

START_TEST (test_LayoutHandler_LineSegment_notes)
{
    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <curve>\n"
      "        <listOfCurveSegments>\n"
      "          <curveSegment xsi:type=\"LineSegment\">\n" 
      "            <notes>"   
                    "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
                    " <p>Testnote</p>\n"
                    "</body>"
                  "</notes>\n"
"                  <start x=\"10\" y=\"15\"/>\n" 
      "            <end x=\"20\" y=\"30\"/>\n" 
      "          </curveSegment>\n"
      "        </listOfCurveSegments>\n"
      "      </curve>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
  );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    fail_unless(rg->isSetCurve());
    const Curve* curve=rg->getCurve();
    fail_unless(curve->getNumCurveSegments()==1);
    const LineSegment* ls=curve->getCurveSegment(0);
    fail_unless(dynamic_cast<const CubicBezier*>(ls)==NULL);
    const Point& start=ls->getStart(); 
    fail_unless(start.getXOffset()==10.0);
    fail_unless(start.getYOffset()==15.0);
    const Point& end=ls->getEnd(); 
    fail_unless(end.getXOffset()==20.0);
    fail_unless(end.getYOffset()==30.0);

    fail_unless(ls->getNotes()==NOTES);
}
END_TEST

START_TEST (test_LayoutHandler_LineSegment_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <curve>\n"
      "        <listOfCurveSegments>\n"
      "          <curveSegment xsi:type=\"LineSegment\">\n" 
      "            <annotation>\n"
      "              <this-is-a-test/>\n"
      "            </annotation>\n"
      "            <start x=\"10\" y=\"15\"/>\n" 
      "            <end x=\"20\" y=\"30\"/>\n" 
      "          </curveSegment>\n"
      "        </listOfCurveSegments>\n"
      "      </curve>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
  );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    fail_unless(rg->isSetCurve());
    const Curve* curve=rg->getCurve();
    fail_unless(curve->getNumCurveSegments()==1);
    const LineSegment* ls=curve->getCurveSegment(0);
    fail_unless(dynamic_cast<const CubicBezier*>(ls)==NULL);
    const Point& start=ls->getStart(); 
    fail_unless(start.getXOffset()==10.0);
    fail_unless(start.getYOffset()==15.0);
    const Point& end=ls->getEnd(); 
    fail_unless(end.getXOffset()==20.0);
    fail_unless(end.getYOffset()==30.0);
}
END_TEST

START_TEST (test_LayoutHandler_CubicBezier)
{
    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <curve>\n"
      "        <listOfCurveSegments>\n"
      "          <curveSegment xsi:type=\"CubicBezier\">\n" 
      "            <start x=\"10\" y=\"15\"/>\n" 
      "            <end x=\"20\" y=\"30\"/>\n" 
      "            <basePoint1 x=\"15\" y=\"5\"/>\n" 
      "            <basePoint2 x=\"15\" y=\"17\"/>\n" 
      "          </curveSegment>\n"
      "        </listOfCurveSegments>\n"
      "      </curve>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
  );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    fail_unless(rg->isSetCurve());
    const Curve* curve=rg->getCurve();
    fail_unless(curve->getNumCurveSegments()==1);
    const LineSegment* ls=curve->getCurveSegment(0);
    const CubicBezier* cb=dynamic_cast<const CubicBezier*>(ls);
    fail_unless(cb!=NULL);
    const Point& start=cb->getStart(); 
    fail_unless(start.getXOffset()==10.0);
    fail_unless(start.getYOffset()==15.0);
    fail_unless(start.getZOffset()==0.0);
    const Point& end=cb->getEnd(); 
    fail_unless(end.getXOffset()==20.0);
    fail_unless(end.getYOffset()==30.0);
    fail_unless(end.getZOffset()==0.0);
    const Point& base1=cb->getBasePoint1(); 
    fail_unless(base1.getXOffset()==15.0);
    fail_unless(base1.getYOffset()==5.0);
    fail_unless(base1.getZOffset()==0.0);
    const Point& base2=cb->getBasePoint2(); 
    fail_unless(base2.getXOffset()==15.0);
    fail_unless(base2.getYOffset()==17.0);
    fail_unless(base2.getZOffset()==0.0);



}
END_TEST

START_TEST (test_LayoutHandler_CubicBezier_notes)
{
    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <curve>\n"
      "        <listOfCurveSegments>\n"
      "          <curveSegment xsi:type=\"CubicBezier\">\n" 
      "            <notes>"   
                    "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
                    " <p>Testnote</p>\n"
                    "</body>"
                  "</notes>\n"
"                  <start x=\"10\" y=\"15\"/>\n" 
      "            <end x=\"20\" y=\"30\"/>\n" 
      "            <basePoint1 x=\"15\" y=\"5\"/>\n" 
      "            <basePoint2 x=\"16\" y=\"17\"/>\n" 
      "          </curveSegment>\n"
      "        </listOfCurveSegments>\n"
      "      </curve>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
  );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    fail_unless(rg->isSetCurve());
    const Curve* curve=rg->getCurve();
    fail_unless(curve->getNumCurveSegments()==1);
    const LineSegment* ls=curve->getCurveSegment(0);
    const CubicBezier* cb=dynamic_cast<const CubicBezier*>(ls);
    fail_unless(cb!=NULL);
    const Point& start=cb->getStart(); 
    fail_unless(start.getXOffset()==10.0);
    fail_unless(start.getYOffset()==15.0);
    fail_unless(start.getZOffset()==0.0);
    const Point& end=cb->getEnd(); 
    fail_unless(end.getXOffset()==20.0);
    fail_unless(end.getYOffset()==30.0);
    fail_unless(end.getZOffset()==0.0);
    const Point& base1=cb->getBasePoint1(); 
    fail_unless(base1.getXOffset()==15.0);
    fail_unless(base1.getYOffset()==5.0);
    fail_unless(base1.getZOffset()==0.0);
    const Point& base2=cb->getBasePoint2(); 
    fail_unless(base2.getXOffset()==16.0);
    fail_unless(base2.getYOffset()==17.0);
    fail_unless(base2.getZOffset()==0.0);

    fail_unless(cb->getNotes()==NOTES);

}
END_TEST

START_TEST (test_LayoutHandler_CubicBezier_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <curve>\n"
      "        <listOfCurveSegments>\n"
      "          <curveSegment xsi:type=\"CubicBezier\">\n" 
      "            <annotation>\n"
      "              <this-is-a-test/>\n"
      "            </annotation>\n"
      "            <start x=\"10\" y=\"15\"/>\n" 
      "            <end x=\"20\" y=\"30\"/>\n" 
      "            <basePoint1 x=\"15\" y=\"5\"/>\n" 
      "            <basePoint2 x=\"16\" y=\"17\"/>\n" 
      "          </curveSegment>\n"
      "        </listOfCurveSegments>\n"
      "      </curve>\n"
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
  );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.0);
    fail_unless(dimensions->getHeight()==400.0);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getId()=="layout_1");

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    fail_unless(rg->isSetCurve());
    const Curve* curve=rg->getCurve();
    fail_unless(curve->getNumCurveSegments()==1);
    const LineSegment* ls=curve->getCurveSegment(0);
    const CubicBezier* cb=dynamic_cast<const CubicBezier*>(ls);
    fail_unless(cb!=NULL);
    const Point& start=cb->getStart(); 
    fail_unless(start.getXOffset()==10.0);
    fail_unless(start.getYOffset()==15.0);
    fail_unless(start.getZOffset()==0.0);
    const Point& end=cb->getEnd(); 
    fail_unless(end.getXOffset()==20.0);
    fail_unless(end.getYOffset()==30.0);
    fail_unless(end.getZOffset()==0.0);
    const Point& base1=cb->getBasePoint1(); 
    fail_unless(base1.getXOffset()==15.0);
    fail_unless(base1.getYOffset()==5.0);
    fail_unless(base1.getZOffset()==0.0);
    const Point& base2=cb->getBasePoint2(); 
    fail_unless(base2.getXOffset()==16.0);
    fail_unless(base2.getYOffset()==17.0);
    fail_unless(base2.getZOffset()==0.0);

}
END_TEST

START_TEST (test_LayoutHandler_Dimensions)
{
    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200.5\" height=\"400.5\" depth=\"455.2\"/>\n" 
      "</layout>\n"
    );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    fail_unless(l->getId()=="layout_1");
    
    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.5);
    fail_unless(dimensions->getHeight()==400.5);
    fail_unless(dimensions->getDepth()==455.2);


}
END_TEST

START_TEST (test_LayoutHandler_Dimensions_notes)
{
    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200.5\" height=\"400.5\" depth=\"455.2\">\n" 
      "    <notes>"   
             "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
             " <p>Testnote</p>\n"
             "</body>"
          "</notes>\n"
      "  </dimensions>\n"
      "</layout>\n"
    );


    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);
    
    fail_unless(l->getId()=="layout_1");

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.5);
    fail_unless(dimensions->getHeight()==400.5);
    fail_unless(dimensions->getDepth()==455.2);

    fail_unless(dimensions->getNotes()==NOTES);


}
END_TEST

START_TEST (test_LayoutHandler_Dimensions_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200.5\" height=\"400.5\" depth=\"455.2\">\n" 
      "    <annotation>\n"
      "      <this-is-a-test/>\n"
      "    </annotation>\n"
      "  </dimensions>\n"
       "</layout>\n"
   );


    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);
    
    fail_unless(l->getId()=="layout_1");

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.5);
    fail_unless(dimensions->getHeight()==400.5);
    fail_unless(dimensions->getDepth()==455.2);


}
END_TEST

START_TEST (test_LayoutHandler_Dimensions_skipOptional)
{
    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "</layout>\n"
    );


    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    fail_unless(l->getId()=="layout_1");

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.5);
    fail_unless(dimensions->getHeight()==400.5);
    fail_unless(dimensions->getDepth()==0.0);


}
END_TEST


START_TEST (test_LayoutHandler_BoundingBox)
{
    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <boundingBox id=\"boundingBox_1\">\n"
      "        <position x=\"10.3\" y=\"20\" z=\"30.23\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\" depth=\"100.2\"/>\n" 
      "      </boundingBox>\n"  
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    fail_unless(l->getId()=="layout_1");

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.5);
    fail_unless(dimensions->getHeight()==400.5);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    const BoundingBox& bb=rg->getBoundingBox();
    fail_unless(bb.getId()=="boundingBox_1");
    
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==30.23);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==100.2); 

}
END_TEST

START_TEST (test_LayoutHandler_BoundingBox_notes)
{
    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <notes>"   
                "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
                " <p>Testnote</p>\n"
                "</body>"
              "</notes>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    fail_unless(l->getId()=="layout_1");

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.5);
    fail_unless(dimensions->getHeight()==400.5);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    const BoundingBox& bb=rg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 

    fail_unless(bb.getNotes()==NOTES);

}
END_TEST

START_TEST (test_LayoutHandler_BoundingBox_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <annotation>\n"
      "          <this-is-a-test/>\n"
      "        </annotation>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    fail_unless(l->getId()=="layout_1");

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.5);
    fail_unless(dimensions->getHeight()==400.5);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    const BoundingBox& bb=rg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 

}
END_TEST

START_TEST (test_LayoutHandler_BoundingBox_skipOptional)
{
    char* s=wrapSBML2
    (
      "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  <listOfReactionGlyphs>\n"
      "    <reactionGlyph id=\"reactionGlyph_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"10.3\" y=\"20\"/>\n"
      "        <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "      </boundingBox>\n"  
      "    </reactionGlyph>\n"
      "  </listOfReactionGlyphs>\n"
      "</layout>\n"
   );

    fail_unless(LH->parse(s,-1,true));

    fail_unless(LISTOFLAYOUTS->getNumItems()==1);

    Layout* l=(Layout*)LISTOFLAYOUTS->get(0);

    fail_unless(l!=NULL);

    fail_unless(l->getId()=="layout_1");

    Dimensions* dimensions=&l->getDimensions();
    fail_unless(dimensions->getWidth()==200.5);
    fail_unless(dimensions->getHeight()==400.5);
    fail_unless(dimensions->getDepth()==0.0);

    fail_unless(l->getNumReactionGlyphs()==1);
    ReactionGlyph* rg=l->getReactionGlyph(0);
    fail_unless(rg->getId()=="reactionGlyph_1");
    fail_unless(!rg->isSetReactionId());

    const BoundingBox& bb=rg->getBoundingBox();
    const Point& position=bb.getPosition();
    fail_unless(position.getXOffset()==10.3);
    fail_unless(position.getYOffset()==20.0);
    fail_unless(position.getZOffset()==0.0);
    const Dimensions& dimensions2=bb.getDimensions();
    fail_unless(dimensions2.getWidth()==200.5); 
    fail_unless(dimensions2.getHeight()==400.5); 
    fail_unless(dimensions2.getDepth()==0.0); 

}
END_TEST


Suite *
create_suite_LayoutHandler (void)
{
  Suite *suite = suite_create("LayoutHandler");
  TCase *tcase = tcase_create("LayoutHandler");

  tcase_add_checked_fixture( tcase,
                             LayoutHandlerTest_setup,
                             LayoutHandlerTest_teardown );


  tcase_add_test( tcase, test_LayoutHandler_Layout                            );
  tcase_add_test( tcase, test_LayoutHandler_Layout_notes                      );
  tcase_add_test( tcase, test_LayoutHandler_Layout_annotation                 );
  tcase_add_test( tcase, test_LayoutHandler_Layout_skipOptional               );
  tcase_add_test( tcase, test_LayoutHandler_CompartmentGlyph                  );
  tcase_add_test( tcase, test_LayoutHandler_CompartmentGlyph_notes            );
  tcase_add_test( tcase, test_LayoutHandler_CompartmentGlyph_annotation       );
  tcase_add_test( tcase, test_LayoutHandler_CompartmentGlyph_skipOptional     );
  tcase_add_test( tcase, test_LayoutHandler_SpeciesGlyph                      );
  tcase_add_test( tcase, test_LayoutHandler_SpeciesGlyph_notes                );
  tcase_add_test( tcase, test_LayoutHandler_SpeciesGlyph_annotation           );
  tcase_add_test( tcase, test_LayoutHandler_SpeciesGlyph_skipOptional         );
  tcase_add_test( tcase, test_LayoutHandler_ReactionGlyph_Curve               );
  tcase_add_test( tcase, test_LayoutHandler_ReactionGlyph_BoundingBox         );
  tcase_add_test( tcase, test_LayoutHandler_ReactionGlyph_notes               );
  tcase_add_test( tcase, test_LayoutHandler_ReactionGlyph_annotation          );
  tcase_add_test( tcase, test_LayoutHandler_ReactionGlyph_skipOptional        );
  tcase_add_test( tcase, test_LayoutHandler_SpeciesReferenceGlyph_Curve       );
  tcase_add_test( tcase, test_LayoutHandler_SpeciesReferenceGlyph_BoundingBox );
  tcase_add_test( tcase, test_LayoutHandler_SpeciesReferenceGlyph_notes       );
  tcase_add_test( tcase, test_LayoutHandler_SpeciesReferenceGlyph_annotation  );
  tcase_add_test( tcase, test_LayoutHandler_SpeciesReferenceGlyph_skipOptional);
  tcase_add_test( tcase, test_LayoutHandler_TextGlyph_text                    );
  tcase_add_test( tcase, test_LayoutHandler_TextGlyph_notes                   );
  tcase_add_test( tcase, test_LayoutHandler_TextGlyph_annotation              );
  tcase_add_test( tcase, test_LayoutHandler_TextGlyph_originOfText            );
  tcase_add_test( tcase, test_LayoutHandler_TextGlyph_skipOptional            );
  tcase_add_test( tcase, test_LayoutHandler_GraphicalObject                   );
  tcase_add_test( tcase, test_LayoutHandler_GraphicalObject_notes             );
  tcase_add_test( tcase, test_LayoutHandler_GraphicalObject_annotation        );
  tcase_add_test( tcase, test_LayoutHandler_Curve                             );
  tcase_add_test( tcase, test_LayoutHandler_Curve_notes                       );
  tcase_add_test( tcase, test_LayoutHandler_Curve_annotation                  );
  tcase_add_test( tcase, test_LayoutHandler_Curve_skipOptional                );
  tcase_add_test( tcase, test_LayoutHandler_LineSegment                       );
  tcase_add_test( tcase, test_LayoutHandler_LineSegment_notes                 );
  tcase_add_test( tcase, test_LayoutHandler_LineSegment_annotation            );
  tcase_add_test( tcase, test_LayoutHandler_CubicBezier                       );
  tcase_add_test( tcase, test_LayoutHandler_CubicBezier_notes                 );
  tcase_add_test( tcase, test_LayoutHandler_CubicBezier_annotation            );
  tcase_add_test( tcase, test_LayoutHandler_Dimensions                        );
  tcase_add_test( tcase, test_LayoutHandler_Dimensions_notes                  );
  tcase_add_test( tcase, test_LayoutHandler_Dimensions_annotation             );
  tcase_add_test( tcase, test_LayoutHandler_Dimensions_skipOptional           );
  tcase_add_test( tcase, test_LayoutHandler_BoundingBox                       );
  tcase_add_test( tcase, test_LayoutHandler_BoundingBox_notes                 );
  tcase_add_test( tcase, test_LayoutHandler_BoundingBox_annotation            );
  tcase_add_test( tcase, test_LayoutHandler_BoundingBox_skipOptional          );
  
  suite_add_tcase(suite, tcase);

  return suite;
}




END_C_DECLS
