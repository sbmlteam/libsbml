/**
 * Filename    : TestTextGlyph.cpp
 * Description : Unit tests for TextGlyph
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


#include "TextGlyph.h"

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

static TextGlyph * TG;

void
TextGlyphTest_setup (void)
{
    TG = new TextGlyph();

    if (TG == NULL)
    {
        fail("new(std::nothrow) TextGlyph() returned a NULL pointer.");
    }

}

void 
TextGlyphTest_teardown (void)
{
    delete TG;
}


CK_CPPSTART

START_TEST ( test_TextGlyph_new )
{
    fail_unless( TG->getTypeCode()    == SBML_LAYOUT_TEXTGLYPH );
    fail_unless( TG->getMetaId()      == "" );
//    fail_unless( TG->getNotes()       == "" );
//    fail_unless( TG->getAnnotation()  == "" );
    fail_unless( TG->getId()          == "" );
    fail_unless( !TG->isSetId());
    fail_unless( !TG->isSetText());
    fail_unless( !TG->isSetGraphicalObjectId());
    fail_unless( !TG->isSetOriginOfTextId());
}
END_TEST

START_TEST ( test_TextGlyph_new_WithLevelVersionAndNamespaces )
{
   unsigned int level=1;
   unsigned int version=2;
   TextGlyph *tg=new TextGlyph(level, version); 
   fail_unless( tg->getTypeCode() == SBML_LAYOUT_TEXTGLYPH );
   fail_unless( tg->getMetaId()   == "" );

   fail_unless(tg->getLevel() == level);
   fail_unless(tg->getVersion() == version);

   fail_unless( tg->isSetId() == false );
   fail_unless( tg->getId()          == "" );
   fail_unless( !tg->isSetId());
   fail_unless( !tg->isSetText());
   fail_unless( !tg->isSetGraphicalObjectId());
   fail_unless( !tg->isSetOriginOfTextId());
   
   delete tg;

   level=2;
   version=3;
   tg=new TextGlyph(level, version); 
   fail_unless( tg->getTypeCode() == SBML_LAYOUT_TEXTGLYPH );
   fail_unless( tg->getMetaId()   == "" );

   fail_unless(tg->getLevel() == level);
   fail_unless(tg->getVersion() == version);

   fail_unless( tg->isSetId() == false );
   fail_unless( tg->getId()          == "" );
   fail_unless( !tg->isSetId());
   fail_unless( !tg->isSetText());
   fail_unless( !tg->isSetGraphicalObjectId());
   fail_unless( !tg->isSetOriginOfTextId());
    
   delete tg;
}
END_TEST

START_TEST ( test_TextGlyph_new_WithNamespace )
{
   SBMLNamespaces* ns = new SBMLNamespaces;
   TextGlyph *tg=new TextGlyph(ns); 
   fail_unless( tg->getTypeCode() == SBML_LAYOUT_TEXTGLYPH );
   fail_unless( tg->getMetaId()   == "" );

   fail_unless(tg->getLevel() == SBML_DEFAULT_LEVEL);
   fail_unless(tg->getVersion() == SBML_DEFAULT_VERSION);

   fail_unless( tg->isSetId() == false );
   fail_unless( tg->getId()          == "" );
   fail_unless( !tg->isSetId());
   fail_unless( !tg->isSetText());
   fail_unless( !tg->isSetGraphicalObjectId());
   fail_unless( !tg->isSetOriginOfTextId());
    
   delete tg;
   delete ns;

   ns = new SBMLNamespaces(2,3);
   tg=new TextGlyph(ns);
   fail_unless( tg->getTypeCode() == SBML_LAYOUT_TEXTGLYPH );
   fail_unless( tg->getMetaId()   == "" );

   fail_unless(tg->getLevel() == 2);
   fail_unless(tg->getVersion() == 3);

   fail_unless( tg->isSetId() == false );
   fail_unless( tg->getId()          == "" );
   fail_unless( !tg->isSetId());
   fail_unless( !tg->isSetText());
   fail_unless( !tg->isSetGraphicalObjectId());
   fail_unless( !tg->isSetOriginOfTextId());
    
   delete tg;
   delete ns;
}
END_TEST


START_TEST ( test_TextGlyph_new_with_text )
{
    std::string id="TestTextGlyphId";
    std::string text="TestTextGlyph";
    TextGlyph* tg=new TextGlyph(id,text);
    fail_unless( tg->getTypeCode()    == SBML_LAYOUT_TEXTGLYPH );
    fail_unless( tg->getMetaId()      == "" );
//    fail_unless( tg->getNotes()       == "" );
//    fail_unless( tg->getAnnotation()  == "" );
    fail_unless( tg->getId()          == id );
    fail_unless( tg->isSetId());
    fail_unless( tg->isSetText());
    fail_unless( tg->getText()        == text );
    fail_unless( !tg->isSetGraphicalObjectId());
    fail_unless( !tg->isSetOriginOfTextId());
    delete tg;
}
END_TEST

START_TEST ( test_TextGlyph_setText )
{
    std::string text="TestTextGlyph"; 
    TG->setText(text);
    fail_unless ( TG->isSetText());
    fail_unless (TG->getText() == text );
}
END_TEST

START_TEST ( test_TextGlyph_setGraphicalObjectId )
{
    std::string id="SomeSpeciesGlyphId"; 
    TG->setGraphicalObjectId(id);
    fail_unless ( TG->isSetGraphicalObjectId());
    fail_unless ( TG->getGraphicalObjectId() == id );
}
END_TEST

START_TEST ( test_TextGlyph_setOriginOfTextId )
{
    std::string id="SomeSpeciesGlyphId"; 
    TG->setOriginOfTextId(id);
    fail_unless ( TG->isSetOriginOfTextId());
    fail_unless ( TG->getOriginOfTextId() == id );
}
END_TEST

START_TEST ( test_TextGlyph_copyConstructor )
{
    TextGlyph* tg1=new TextGlyph();
    XMLNode* notes=new XMLNode();
    tg1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    tg1->setAnnotation(annotation);
    TextGlyph* tg2=new TextGlyph(*tg1);
    delete tg2;
    delete tg1;
}
END_TEST

START_TEST ( test_TextGlyph_assignmentOperator )
{
    TextGlyph* tg1=new TextGlyph();
    XMLNode* notes=new XMLNode();
    tg1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    tg1->setAnnotation(annotation);
    TextGlyph* tg2=new TextGlyph();
    (*tg2)=(*tg1);
    delete tg2;
    delete tg1;
}
END_TEST

Suite *
create_suite_TextGlyph (void)
{
  Suite *suite = suite_create("TextGlyph");
  TCase *tcase = tcase_create("TextGlyph");

  tcase_add_checked_fixture( tcase,
                             TextGlyphTest_setup,
                             TextGlyphTest_teardown );


  tcase_add_test(tcase , test_TextGlyph_new                               );
  tcase_add_test( tcase, test_TextGlyph_new_WithLevelVersionAndNamespaces );
  tcase_add_test( tcase, test_TextGlyph_new_WithNamespace                 );
  tcase_add_test(tcase , test_TextGlyph_new_with_text                     );
  tcase_add_test(tcase , test_TextGlyph_setText                           );
  tcase_add_test(tcase , test_TextGlyph_setGraphicalObjectId              );
  tcase_add_test(tcase , test_TextGlyph_setOriginOfTextId                 );
  tcase_add_test( tcase, test_TextGlyph_copyConstructor                   );
  tcase_add_test( tcase, test_TextGlyph_assignmentOperator                );
  
  suite_add_tcase(suite, tcase);

  return suite;
}




CK_CPPEND
