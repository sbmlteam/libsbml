/**
 * Filename    : TestLayoutFormatter.cpp
 * Description : Unit tests for LayoutFormatter
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

#include "LayoutFormatter.h"
#include "Layout.h"
#include "GraphicalObject.h"
#include "CompartmentGlyph.h"
#include "SpeciesGlyph.h"
#include "ReactionGlyph.h"
#include "TextGlyph.h"
#include "SpeciesReferenceGlyph.h"
#include "LineSegment.h"
#include "CubicBezier.h"
#include "Curve.h"
#include "Point.h"
#include "Dimensions.h"


BEGIN_C_DECLS

#ifndef USE_EXPAT
XERCES_CPP_NAMESPACE_USE
#endif /* !USE_EXPAT */

#define XML_HEADER   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
#define wrapXML(s)   XML_HEADER s

static MemBufFormatTarget *target;
static LayoutFormatter *LF;

void
LayoutFormatterTest_setup (void)
{
  try
  {
    XML_PLATFORM_UTILS_INIT();
  }
  catch (...)
  {
    fail("XMLPlatformUtils::Initialize() threw an Exception.");
  }

    target    = new MemBufFormatTarget();
    LF = new(std::nothrow )LayoutFormatter(target);

    if (LF == NULL)
    {
        fail("new(std::nothrow) LayoutFormatter() returned a NULL pointer.");
    }

}

void 
LayoutFormatterTest_teardown (void)
{
    delete LF;
    delete target;
}


START_TEST (test_LayoutFormatter_Layout)
{
    const char* s = wrapXML
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

    Dimensions dim=Dimensions(200.0,400.0);
    Layout l("layout_1",dim);
    CompartmentGlyph* cg=l.createCompartmentGlyph();
    cg->setId("compartmentGlyph_1");
    SpeciesGlyph* sg=l.createSpeciesGlyph();
    sg->setId("speciesGlyph_1");
    ReactionGlyph* rg=l.createReactionGlyph();
    rg->setId("reactionGlyph_1");
    TextGlyph* tg=l.createTextGlyph();
    tg->setId("textGlyph_1");
    tg->setText("test");
    GraphicalObject* go=l.createAdditionalGraphicalObject();
    go->setId("graphicalObject_1");

    *LF << l;

    fail_unless( !strcmp((char*)target->getRawBuffer() , s), NULL );   
}
END_TEST

START_TEST (test_LayoutFormatter_Layout_notes)
{
    const char* s = wrapXML
    (
      "<layout id=\"layout_1\">\n"
      "  <notes>\n"
      "    Test note.\n"
      "</notes>\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "</layout>\n"     
    );

    Dimensions dim=Dimensions(200.0,400.0);
    Layout l("layout_1",dim);
    l.setNotes("Test note.");
    
    *LF << l;

    fail_unless( !strcmp((char*)target->getRawBuffer() , s), NULL );   
}
END_TEST

START_TEST (test_LayoutFormatter_Layout_annotation)
{
    const char* s = wrapXML
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

    Dimensions dim=Dimensions(200.0,400.0);
    Layout l("layout_1",dim);
    l.setAnnotation(a);
    
    *LF << l;

    fail_unless( !strcmp((char*)target->getRawBuffer() , s), NULL );   
}
END_TEST

START_TEST (test_LayoutFormatter_Layout_skipOptional)
{
    const char* s = wrapXML
    (
      "<layout id=\"layout_1\">\n"
      "  <dimensions width=\"200\" height=\"400\"/>\n" 
      "</layout>\n"     
    );

    Dimensions dim=Dimensions(200.0,400.0);
    Layout l("layout_1",dim);

    *LF << l;

    fail_unless( !strcmp((char*)target->getRawBuffer() , s), NULL );   
}
END_TEST


START_TEST (test_LayoutFormatter_CompartmentGlyph)
{
    char* s=wrapXML
    (
      "<compartmentGlyph id=\"compartmentGlyph_1\" compartment=\"compartment_1\">\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</compartmentGlyph>\n"
    );
    
    
    CompartmentGlyph cg=CompartmentGlyph();
    cg.setId("compartmentGlyph_1");
    cg.setCompartmentId("compartment_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    cg.setBoundingBox(box);

    *LF << cg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);

}
END_TEST

START_TEST (test_LayoutFormatter_CompartmentGlyph_notes)
{
    char* s=wrapXML
    (
      "<compartmentGlyph id=\"compartmentGlyph_1\">\n"
      "  <notes>\n"
      "    Test note.\n"
      "</notes>\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</compartmentGlyph>\n"
    );
    
    
    CompartmentGlyph cg=CompartmentGlyph();
    cg.setId("compartmentGlyph_1");
    cg.setNotes("Test note.");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    cg.setBoundingBox(box);

    *LF << cg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_CompartmentGlyph_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapXML
    (
      "<compartmentGlyph id=\"compartmentGlyph_1\">\n"
      "  <annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</compartmentGlyph>\n"
    );
    
    
    CompartmentGlyph cg=CompartmentGlyph();
    cg.setId("compartmentGlyph_1");
    cg.setAnnotation(a);
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    cg.setBoundingBox(box);

    *LF << cg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_CompartmentGlyph_skipOptional)
{
    char* s=wrapXML
    (
      "<compartmentGlyph id=\"compartmentGlyph_1\">\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</compartmentGlyph>\n"
    );
    
    
    CompartmentGlyph cg=CompartmentGlyph();
    cg.setId("compartmentGlyph_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    cg.setBoundingBox(box);

    *LF << cg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_SpeciesGlyph)
{
    char* s=wrapXML
    (
      "<speciesGlyph id=\"speciesGlyph_1\" species=\"species_1\">\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</speciesGlyph>\n"
    );
    
    
    SpeciesGlyph sg=SpeciesGlyph();
    sg.setId("speciesGlyph_1");
    sg.setSpeciesId("species_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    sg.setBoundingBox(box);

    *LF << sg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);

}
END_TEST

START_TEST (test_LayoutFormatter_SpeciesGlyph_notes)
{
    char* s=wrapXML
    (
      "<speciesGlyph id=\"speciesGlyph_1\">\n"
      "  <notes>\n"
      "    Test note.\n"
      "</notes>\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</speciesGlyph>\n"
    );
    
    
    SpeciesGlyph sg=SpeciesGlyph();
    sg.setId("speciesGlyph_1");
    sg.setNotes("Test note.");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    sg.setBoundingBox(box);

    *LF << sg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_SpeciesGlyph_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapXML
    (
      "<speciesGlyph id=\"speciesGlyph_1\">\n"
      "  <annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</speciesGlyph>\n"
    );
    
    
    SpeciesGlyph sg=SpeciesGlyph();
    sg.setId("speciesGlyph_1");
    sg.setAnnotation(a);
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    sg.setBoundingBox(box);

    *LF << sg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_SpeciesGlyph_skipOptional)
{
    char* s=wrapXML
    (
      "<speciesGlyph id=\"speciesGlyph_1\">\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</speciesGlyph>\n"
    );
    
    
    SpeciesGlyph sg=SpeciesGlyph();
    sg.setId("speciesGlyph_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    sg.setBoundingBox(box);

    *LF << sg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_ReactionGlyph_Curve)
{
    char* s=wrapXML
    (
      "<reactionGlyph id=\"reactionGlyph_1\" reaction=\"reaction_1\">\n"
      "  <curve>\n"
      "    <listOfCurveSegments>\n"
      "      <curveSegment xsi:type=\"LineSegment\">\n" 
      "        <start x=\"10\" y=\"10\"/>\n" 
      "        <end x=\"20\" y=\"10\"/>\n" 
      "      </curveSegment>\n"
      "    </listOfCurveSegments>\n"
      "  </curve>\n"
      "</reactionGlyph>\n"
    );
    
    
    ReactionGlyph rg=ReactionGlyph();
    rg.setId("reactionGlyph_1");
    rg.setReactionId("reaction_1");
    LineSegment* ls=&rg.createLineSegment();
    ls->setStart(10.0,10.0);
    ls->setEnd(20.0,10.0);

    *LF << rg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_ReactionGlyph_BoundingBox)
{
    char* s=wrapXML
    (
      "<reactionGlyph id=\"reactionGlyph_1\" reaction=\"reaction_1\">\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</reactionGlyph>\n"
    );
    
    
    ReactionGlyph rg=ReactionGlyph();
    rg.setId("reactionGlyph_1");
    rg.setReactionId("reaction_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    rg.setBoundingBox(box);

    *LF << rg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);

}
END_TEST

START_TEST (test_LayoutFormatter_ReactionGlyph_notes)
{
    char* s=wrapXML
    (
      "<reactionGlyph id=\"reactionGlyph_1\" reaction=\"reaction_1\">\n"
      "  <notes>\n"
      "    Test note.\n"
      "</notes>\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</reactionGlyph>\n"
    );
    
    
    ReactionGlyph rg=ReactionGlyph();
    rg.setId("reactionGlyph_1");
    rg.setNotes("Test note.");
    rg.setReactionId("reaction_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    rg.setBoundingBox(box);

    *LF << rg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_ReactionGlyph_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapXML
    (
      "<reactionGlyph id=\"reactionGlyph_1\" reaction=\"reaction_1\">\n"
      "  <annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</reactionGlyph>\n"
    );
    
    
    ReactionGlyph rg=ReactionGlyph();
    rg.setId("reactionGlyph_1");
    rg.setReactionId("reaction_1");
    rg.setAnnotation(a);
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    rg.setBoundingBox(box);

    *LF << rg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_ReactionGlyph_skipOptional)
{
    char* s=wrapXML
    (
      "<reactionGlyph id=\"reactionGlyph_1\">\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</reactionGlyph>\n"
    );
    
    
    ReactionGlyph rg=ReactionGlyph();
    rg.setId("reactionGlyph_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    rg.setBoundingBox(box);

    *LF << rg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_SpeciesReferenceGlyph_Curve)
{
    char* s=wrapXML
    (
      "<speciesReferenceGlyph id=\"speciesReferenceGlyph_1\" speciesReference=\"speciesReference_1\" speciesGlyph=\"speciesGlyph_1\" role=\"undefined\">\n"
      "  <curve>\n"
      "    <listOfCurveSegments>\n"
      "      <curveSegment xsi:type=\"LineSegment\">\n" 
      "        <start x=\"10\" y=\"10\"/>\n" 
      "        <end x=\"20\" y=\"10\"/>\n" 
      "      </curveSegment>\n"
      "    </listOfCurveSegments>\n"
      "  </curve>\n"
      "</speciesReferenceGlyph>\n"
    );
    
    
    SpeciesReferenceGlyph srg=SpeciesReferenceGlyph();
    srg.setId("speciesReferenceGlyph_1");
    srg.setSpeciesGlyphId("speciesGlyph_1");
    srg.setSpeciesReferenceId("speciesReference_1");
    LineSegment* ls=&srg.createLineSegment();
    ls->setStart(10.0,10.0);
    ls->setEnd(20.0,10.0);

    *LF << srg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);

}
END_TEST

START_TEST (test_LayoutFormatter_SpeciesReferenceGlyph_BoundingBox)
{
    char* s=wrapXML
    (
      "<speciesReferenceGlyph id=\"speciesReferenceGlyph_1\" speciesReference=\"speciesReference_1\" speciesGlyph=\"speciesGlyph_1\" role=\"undefined\">\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</speciesReferenceGlyph>\n"
    );
    
    
    SpeciesReferenceGlyph srg=SpeciesReferenceGlyph();
    srg.setId("speciesReferenceGlyph_1");
    srg.setSpeciesGlyphId("speciesGlyph_1");
    srg.setSpeciesReferenceId("speciesReference_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    srg.setBoundingBox(box);

    *LF << srg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);

}
END_TEST

START_TEST (test_LayoutFormatter_SpeciesReferenceGlyph_notes)
{
    char* s=wrapXML
    (
      "<speciesReferenceGlyph id=\"speciesReferenceGlyph_1\" role=\"undefined\">\n"
      "  <notes>\n"
      "    Test note.\n"
      "</notes>\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</speciesReferenceGlyph>\n"
    );
    
    
    SpeciesReferenceGlyph srg=SpeciesReferenceGlyph();
    srg.setNotes("Test note.");
    srg.setId("speciesReferenceGlyph_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    srg.setBoundingBox(box);

    *LF << srg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_SpeciesReferenceGlyph_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapXML
    (
      "<speciesReferenceGlyph id=\"speciesReferenceGlyph_1\" role=\"undefined\">\n"
      "  <annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</speciesReferenceGlyph>\n"
    );
    
    
    SpeciesReferenceGlyph srg=SpeciesReferenceGlyph();
    srg.setId("speciesReferenceGlyph_1");
    srg.setAnnotation(a);
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    srg.setBoundingBox(box);

    *LF << srg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_SpeciesReferenceGlyph_skipOptional)
{
    char* s=wrapXML
    (
      "<speciesReferenceGlyph id=\"speciesReferenceGlyph_1\" role=\"undefined\">\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</speciesReferenceGlyph>\n"
    );
    
    
    SpeciesReferenceGlyph srg=SpeciesReferenceGlyph();
    srg.setId("speciesReferenceGlyph_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    srg.setBoundingBox(box);

    *LF << srg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_TextGlyph_text)
{
    char* s=wrapXML
    (
      "<textGlyph id=\"textGlyph_1\" graphicalObject=\"speciesGlyph_1\" text=\"test text\">\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</textGlyph>\n"
    );
    
    
    TextGlyph tg=TextGlyph();
    tg.setId("textGlyph_1");
    tg.setText("test text");
    tg.setGraphicalObjectId("speciesGlyph_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    tg.setBoundingBox(box);

    *LF << tg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);

}
END_TEST

START_TEST (test_LayoutFormatter_TextGlyph_originOfText)
{
    char* s=wrapXML
    (
      "<textGlyph id=\"textGlyph_1\" graphicalObject=\"speciesGlyph_1\" originOfText=\"reactionGlyph_1\">\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</textGlyph>\n"
    );
    
    
    TextGlyph tg=TextGlyph();
    tg.setId("textGlyph_1");
    tg.setOriginOfTextId("reactionGlyph_1");
    tg.setGraphicalObjectId("speciesGlyph_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    tg.setBoundingBox(box);

    *LF << tg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);

}
END_TEST

START_TEST (test_LayoutFormatter_TextGlyph_notes)
{
    char* s=wrapXML
    (
      "<textGlyph id=\"textGlyph_1\" graphicalObject=\"speciesGlyph_1\" originOfText=\"reactionGlyph_1\">\n"
      "  <notes>\n"
      "    Test note.\n"
      "</notes>\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</textGlyph>\n"
    );
    
    
    TextGlyph tg=TextGlyph();
    tg.setId("textGlyph_1");
    tg.setNotes("Test note.");
    tg.setOriginOfTextId("reactionGlyph_1");
    tg.setGraphicalObjectId("speciesGlyph_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    tg.setBoundingBox(box);

    *LF << tg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);

}
END_TEST

START_TEST (test_LayoutFormatter_TextGlyph_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapXML
    (
      "<textGlyph id=\"textGlyph_1\" graphicalObject=\"speciesGlyph_1\" originOfText=\"reactionGlyph_1\">\n"
      "  <annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</textGlyph>\n"
    );
    
    
    TextGlyph tg=TextGlyph();
    tg.setId("textGlyph_1");
    tg.setOriginOfTextId("reactionGlyph_1");
    tg.setGraphicalObjectId("speciesGlyph_1");
    tg.setAnnotation(a);
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    tg.setBoundingBox(box);

    *LF << tg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);

}
END_TEST

START_TEST (test_LayoutFormatter_TextGlyph_skipOptional)
{
    char* s=wrapXML
    (
      "<textGlyph id=\"textGlyph_1\">\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</textGlyph>\n"
    );
    
    
    TextGlyph tg=TextGlyph();
    tg.setId("textGlyph_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    tg.setBoundingBox(box);

    *LF << tg;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);

}
END_TEST

START_TEST (test_LayoutFormatter_GraphicalObject)
{
    char* s=wrapXML
    (
      "<graphicalObject id=\"graphicalObject_1\">\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</graphicalObject>\n"
    );
    
    
    GraphicalObject go=GraphicalObject();
    go.setId("graphicalObject_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    go.setBoundingBox(box);

    *LF << go;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);

}
END_TEST

START_TEST (test_LayoutFormatter_GraphicalObject_notes)
{
    char* s=wrapXML
    (
      "<graphicalObject id=\"graphicalObject_1\">\n"
      "  <notes>\n"
      "    Test note.\n"
      "</notes>\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</graphicalObject>\n"
    );
    
    
    GraphicalObject go=GraphicalObject();
    go.setNotes("Test note.");
    go.setId("graphicalObject_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    go.setBoundingBox(box);

    *LF << go;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);

}
END_TEST

START_TEST (test_LayoutFormatter_GraphicalObject_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapXML
    (
      "<graphicalObject id=\"graphicalObject_1\">\n"
      "  <annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>\n"
      "  <boundingBox>\n"
      "    <position x=\"10.3\" y=\"20\"/>\n"
      "    <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "  </boundingBox>\n"  
      "</graphicalObject>\n"
    );
    
    
    GraphicalObject go=GraphicalObject();
    go.setAnnotation(a);
    go.setId("graphicalObject_1");
    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    go.setBoundingBox(box);

    *LF << go;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s) ,NULL);

}
END_TEST

START_TEST (test_LayoutFormatter_Curve)
{
    char* s=wrapXML
    (
      "<curve>\n"
      "  <listOfCurveSegments>\n"
      "    <curveSegment xsi:type=\"LineSegment\">\n" 
      "      <start x=\"10\" y=\"10\"/>\n" 
      "      <end x=\"20\" y=\"10\"/>\n" 
      "    </curveSegment>\n"
      "  </listOfCurveSegments>\n"
      "</curve>\n"
    );

    Curve c=Curve();
    LineSegment* ls=&c.createLineSegment();
    ls->setStart(10.0,10.0);
    ls->setEnd(20.0,10.0);
    
    *LF << c;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
    
}
END_TEST

START_TEST (test_LayoutFormatter_Curve_notes)
{
    char* s=wrapXML
    (
      "<curve>\n"
      "  <notes>\n"
      "    Test note.\n"
      "</notes>\n"
      "  <listOfCurveSegments>\n"
      "    <curveSegment xsi:type=\"LineSegment\">\n" 
      "      <start x=\"10\" y=\"10\"/>\n" 
      "      <end x=\"20\" y=\"10\"/>\n" 
      "    </curveSegment>\n"
      "  </listOfCurveSegments>\n"
      "</curve>\n"
    );

    Curve c=Curve();
    c.setNotes("Test note.");
    LineSegment* ls=&c.createLineSegment();
    ls->setStart(10.0,10.0);
    ls->setEnd(20.0,10.0);
    
    
    *LF << c;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
 }
END_TEST

START_TEST (test_LayoutFormatter_Curve_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapXML
    (
      "<curve>\n"
      "  <annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>\n"
      "  <listOfCurveSegments>\n"
      "    <curveSegment xsi:type=\"LineSegment\">\n" 
      "      <start x=\"10\" y=\"10\"/>\n" 
      "      <end x=\"20\" y=\"10\"/>\n" 
      "    </curveSegment>\n"
      "  </listOfCurveSegments>\n"
      "</curve>\n"
    );

    Curve c=Curve();
    c.setAnnotation(a);
    LineSegment* ls=&c.createLineSegment();
    ls->setStart(10.0,10.0);
    ls->setEnd(20.0,10.0);
    
    *LF << c;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
 }
END_TEST

START_TEST (test_LayoutFormatter_Curve_skipOptional)
{
    char* s=wrapXML
    (
      "<curve>\n"
      "</curve>\n"
    );

    Curve c=Curve();
    
    *LF << c;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
 }
END_TEST

START_TEST (test_LayoutFormatter_LineSegment)
{
    char* s=wrapXML
    (
      "<curveSegment xsi:type=\"LineSegment\">\n" 
      "  <start x=\"10\" y=\"10\"/>\n" 
      "  <end x=\"20\" y=\"10\"/>\n" 
      "</curveSegment>\n"
    );

    LineSegment ls=LineSegment();
    ls.setStart(10.0,10.0);
    ls.setEnd(20.0,10.0);
    
    *LF << ls;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_LineSegment_notes)
{
    char* s=wrapXML
    (
      "<curveSegment xsi:type=\"LineSegment\">\n" 
      "  <notes>\n"
      "    Test note.\n"
      "</notes>\n"
      "  <start x=\"10\" y=\"10\"/>\n" 
      "  <end x=\"20\" y=\"10\"/>\n" 
      "</curveSegment>\n"
    );

    LineSegment ls=LineSegment();
    ls.setStart(10.0,10.0);
    ls.setEnd(20.0,10.0);
    ls.setNotes("Test note.");
    
    *LF << ls;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_LineSegment_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapXML
    (
      "<curveSegment xsi:type=\"LineSegment\">\n" 
      "  <annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>\n"
      "  <start x=\"10\" y=\"10\"/>\n" 
      "  <end x=\"20\" y=\"10\"/>\n" 
      "</curveSegment>\n"
    );

    LineSegment ls=LineSegment();
    ls.setStart(10.0,10.0);
    ls.setEnd(20.0,10.0);
    ls.setAnnotation(a);
    
    *LF << ls;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_CubicBezier)
{
    char* s=wrapXML
    (
      "<curveSegment xsi:type=\"CubicBezier\">\n" 
      "  <start x=\"10\" y=\"10\"/>\n" 
      "  <end x=\"20\" y=\"10\"/>\n" 
      "  <basePoint1 x=\"15\" y=\"5\"/>\n" 
      "  <basePoint2 x=\"15\" y=\"15\"/>\n" 
      "</curveSegment>\n"
    );

    CubicBezier cb=CubicBezier();
    cb.setStart(10.0,10.0);
    cb.setEnd(20.0,10.0);
    cb.setBasePoint1(15.0,5.0);
    cb.setBasePoint2(15.0,15.0);
    
    *LF << cb;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_CubicBezier_notes)
{
    char* s=wrapXML
    (
      "<curveSegment xsi:type=\"CubicBezier\">\n" 
      "  <notes>\n"
      "    Test note.\n"
      "</notes>\n"
      "  <start x=\"10\" y=\"10\"/>\n" 
      "  <end x=\"20\" y=\"10\"/>\n" 
      "  <basePoint1 x=\"15\" y=\"5\"/>\n" 
      "  <basePoint2 x=\"15\" y=\"15\"/>\n" 
      "</curveSegment>\n"
    );

    CubicBezier cb=CubicBezier();
    cb.setStart(10.0,10.0);
    cb.setEnd(20.0,10.0);
    cb.setBasePoint1(15.0,5.0);
    cb.setBasePoint2(15.0,15.0);
    cb.setNotes("Test note.");
    
    *LF << cb;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_CubicBezier_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapXML
    (
      "<curveSegment xsi:type=\"CubicBezier\">\n" 
      "  <annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>\n"
      "  <start x=\"10\" y=\"10\"/>\n" 
      "  <end x=\"20\" y=\"10\"/>\n" 
      "  <basePoint1 x=\"15\" y=\"5\"/>\n" 
      "  <basePoint2 x=\"15\" y=\"15\"/>\n" 
      "</curveSegment>\n"
    );

    CubicBezier cb=CubicBezier();
    cb.setStart(10.0,10.0);
    cb.setEnd(20.0,10.0);
    cb.setBasePoint1(15.0,5.0);
    cb.setBasePoint2(15.0,15.0);
    cb.setAnnotation(a);
    
    *LF << cb;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST

/*
START_TEST (test_LayoutFormatter_Point)
{
    char* s=wrapXML
    (
      "<point x=\"200.5\" y=\"400.5\" z=\"455.2\"/>\n" 
    );

    Point p=Point(200.5,400.5,455.2);

    *LF << p;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_Point_notes)
{
    char* s=wrapXML
    (
      "<point x=\"200.5\" y=\"400.5\" z=\"455.2\">\n" 
      "  <notes>\n"
      "    Test note.\n"
      "</notes>\n"
      "</point>\n"
    );

    Point p=Point(200.5,400.5,455.2);
    p.setNotes("Test note.");

    *LF << p;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_Point_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapXML
    (
      "<point x=\"200.5\" y=\"400.5\" z=\"455.2\">\n" 
      "  <annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>\n"
      "</point>\n"
    );

    Point p=Point(200.5,400.5,455.2);
    p.setAnnotation(a);

    *LF << p;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_Point_skipOptional)
{
    char* s=wrapXML
    (
      "<point x=\"200.5\" y=\"400.5\"/>\n" 
    );

    Point p=Point(200.5,400.5);

    *LF << p;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST
*/

START_TEST (test_LayoutFormatter_Dimensions)
{
    char* s=wrapXML
    (
      "<dimensions width=\"200.5\" height=\"400.5\" depth=\"455.2\"/>\n" 
    );

    Dimensions dim=Dimensions(200.5,400.5,455.2);

    *LF << dim;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_Dimensions_notes)
{
    char* s=wrapXML
    (
      "<dimensions width=\"200.5\" height=\"400.5\" depth=\"455.2\">\n" 
      "  <notes>\n"
      "    Test note.\n"
      "</notes>\n"
      "</dimensions>\n"
    );

    Dimensions dim=Dimensions(200.5,400.5,455.2);
    dim.setNotes("Test note.");

    *LF << dim;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_Dimensions_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapXML
    (
      "<dimensions width=\"200.5\" height=\"400.5\" depth=\"455.2\">\n" 
      "  <annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>\n"
      "</dimensions>\n"
    );

    Dimensions dim=Dimensions(200.5,400.5,455.2);
    dim.setAnnotation(a);

    *LF << dim;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_Dimensions_skipOptional)
{
    char* s=wrapXML
    (
      "<dimensions width=\"200.5\" height=\"400.5\"/>\n" 
    );

    Dimensions dim=Dimensions(200.5,400.5);

    *LF << dim;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST


START_TEST (test_LayoutFormatter_BoundingBox)
{
    char* s=wrapXML
    (
      "<boundingBox id=\"boundingBox_1\">\n"
      "  <position x=\"10.3\" y=\"20\"/>\n"
      "  <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "</boundingBox>\n"  
    );

    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("boundingBox_1",pos,dim);

    *LF << box;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_BoundingBox_notes)
{
    char* s=wrapXML
    (
      "<boundingBox>\n"
      "  <notes>\n"
      "    Test note.\n"
      "</notes>\n"
      "  <position x=\"10.3\" y=\"20\"/>\n"
      "  <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "</boundingBox>\n"  
    );

    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);
    box.setNotes("Test note.");

    *LF << box;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST

START_TEST (test_LayoutFormatter_BoundingBox_annotation)
{
    const char* a =
      "<annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>";

    char* s=wrapXML
    (
      "<boundingBox>\n"
      "  <annotation>\n"
      "    <this-is-a-test/>\n"
      "  </annotation>\n"
      "  <position x=\"10.3\" y=\"20\"/>\n"
      "  <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "</boundingBox>\n"  
    );

    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);

    box.setAnnotation(a);

    *LF << box;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);

}
END_TEST

START_TEST (test_LayoutFormatter_BoundingBox_skipOptional)
{
    char* s=wrapXML
    (
      "<boundingBox>\n"
      "  <position x=\"10.3\" y=\"20\"/>\n"
      "  <dimensions width=\"200.5\" height=\"400.5\"/>\n" 
      "</boundingBox>\n"  
    );

    Dimensions dim=Dimensions(200.5,400.5);
    Point pos=Point(10.3,20.0);
    BoundingBox box=BoundingBox("",pos,dim);

    *LF << box;

    fail_unless ( !strcmp((char*)target->getRawBuffer(),s),NULL);
}
END_TEST


Suite *
create_suite_LayoutFormatter (void)
{
  Suite *suite = suite_create("LayoutFormatter");
  TCase *tcase = tcase_create("LayoutFormatter");

  tcase_add_checked_fixture( tcase,
                             LayoutFormatterTest_setup,
                             LayoutFormatterTest_teardown );


  tcase_add_test( tcase, test_LayoutFormatter_Layout                            );
  tcase_add_test( tcase, test_LayoutFormatter_Layout_notes                      );
  tcase_add_test( tcase, test_LayoutFormatter_Layout_annotation                 );
  tcase_add_test( tcase, test_LayoutFormatter_Layout_skipOptional               );
  tcase_add_test( tcase, test_LayoutFormatter_CompartmentGlyph                  );
  tcase_add_test( tcase, test_LayoutFormatter_CompartmentGlyph_notes            );
  tcase_add_test( tcase, test_LayoutFormatter_CompartmentGlyph_annotation       );
  tcase_add_test( tcase, test_LayoutFormatter_CompartmentGlyph_skipOptional     );
  tcase_add_test( tcase, test_LayoutFormatter_SpeciesGlyph                      );
  tcase_add_test( tcase, test_LayoutFormatter_SpeciesGlyph_notes                );
  tcase_add_test( tcase, test_LayoutFormatter_SpeciesGlyph_annotation           );
  tcase_add_test( tcase, test_LayoutFormatter_SpeciesGlyph_skipOptional         );
  tcase_add_test( tcase, test_LayoutFormatter_ReactionGlyph_Curve               );
  tcase_add_test( tcase, test_LayoutFormatter_ReactionGlyph_BoundingBox         );
  tcase_add_test( tcase, test_LayoutFormatter_ReactionGlyph_notes               );
  tcase_add_test( tcase, test_LayoutFormatter_ReactionGlyph_annotation          );
  tcase_add_test( tcase, test_LayoutFormatter_ReactionGlyph_skipOptional        );
  tcase_add_test( tcase, test_LayoutFormatter_SpeciesReferenceGlyph_Curve       );
  tcase_add_test( tcase, test_LayoutFormatter_SpeciesReferenceGlyph_BoundingBox );
  tcase_add_test( tcase, test_LayoutFormatter_SpeciesReferenceGlyph_notes       );
  tcase_add_test( tcase, test_LayoutFormatter_SpeciesReferenceGlyph_annotation  );
  tcase_add_test( tcase, test_LayoutFormatter_SpeciesReferenceGlyph_skipOptional);
  tcase_add_test( tcase, test_LayoutFormatter_TextGlyph_text                    );
  tcase_add_test( tcase, test_LayoutFormatter_TextGlyph_notes                   );
  tcase_add_test( tcase, test_LayoutFormatter_TextGlyph_annotation              );
  tcase_add_test( tcase, test_LayoutFormatter_TextGlyph_originOfText            );
  tcase_add_test( tcase, test_LayoutFormatter_TextGlyph_skipOptional            );
  tcase_add_test( tcase, test_LayoutFormatter_GraphicalObject                   );
  tcase_add_test( tcase, test_LayoutFormatter_GraphicalObject_notes             );
  tcase_add_test( tcase, test_LayoutFormatter_GraphicalObject_annotation        );
  tcase_add_test( tcase, test_LayoutFormatter_Curve                             );
  tcase_add_test( tcase, test_LayoutFormatter_Curve_notes                       );
  tcase_add_test( tcase, test_LayoutFormatter_Curve_annotation                  );
  tcase_add_test( tcase, test_LayoutFormatter_Curve_skipOptional                );
  tcase_add_test( tcase, test_LayoutFormatter_LineSegment                       );
  tcase_add_test( tcase, test_LayoutFormatter_LineSegment_notes                 );
  tcase_add_test( tcase, test_LayoutFormatter_LineSegment_annotation            );
  tcase_add_test( tcase, test_LayoutFormatter_CubicBezier                       );
  tcase_add_test( tcase, test_LayoutFormatter_CubicBezier_notes                 );
  tcase_add_test( tcase, test_LayoutFormatter_CubicBezier_annotation            );
//  tcase_add_test( tcase, test_LayoutFormatter_Point                             );
//  tcase_add_test( tcase, test_LayoutFormatter_Point_notes                       );
//  tcase_add_test( tcase, test_LayoutFormatter_Point_annotation                  );
//  tcase_add_test( tcase, test_LayoutFormatter_Point_skipOptional                );
  tcase_add_test( tcase, test_LayoutFormatter_Dimensions                        );
  tcase_add_test( tcase, test_LayoutFormatter_Dimensions_notes                  );
  tcase_add_test( tcase, test_LayoutFormatter_Dimensions_annotation             );
  tcase_add_test( tcase, test_LayoutFormatter_Dimensions_skipOptional           );
  tcase_add_test( tcase, test_LayoutFormatter_BoundingBox                       );
  tcase_add_test( tcase, test_LayoutFormatter_BoundingBox_notes                 );
  tcase_add_test( tcase, test_LayoutFormatter_BoundingBox_annotation            );
  tcase_add_test( tcase, test_LayoutFormatter_BoundingBox_skipOptional          );
  
  suite_add_tcase(suite, tcase);

  return suite;
}



END_C_DECLS
