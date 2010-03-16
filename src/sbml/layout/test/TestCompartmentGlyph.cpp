/**
 * Filename    : TestCompartmentGlyph.cpp
 * Description : Unit tests for the CompartmentGlyph
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

#include <string>

#include <common/common.h>
#include <common/extern.h>

#include "CompartmentGlyph.h"

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static CompartmentGlyph * CG;

void
CompartmentGlyphTest_setup (void)
{
    CG = new(std::nothrow )CompartmentGlyph();

    if (CG == NULL)
    {
        fail("new(std::nothrow) CompartmentGlyph() returned a NULL pointer.");
    }

}

void 
CompartmentGlyphTest_teardown (void)
{
    delete CG;
}

START_TEST ( test_CompartmentGlyph_new )
{
    fail_unless( CG->getTypeCode()    == SBML_LAYOUT_COMPARTMENTGLYPH );
    fail_unless( CG->getMetaId()      == "" );
//    fail_unless( CG->getNotes()       == "" );
//    fail_unless( CG->getAnnotation()  == "" );
    fail_unless( CG->getId()          == "" );
    fail_unless( !CG->isSetId());
    fail_unless( !CG->isSetCompartmentId());
}
END_TEST

START_TEST ( test_CompartmentGlyph_new_WithLevelVersionAndNamespaces )
{
   unsigned int level=1;
   unsigned int version=2;
   CompartmentGlyph *cg=new CompartmentGlyph(level, version); 
   fail_unless( cg->getTypeCode() == SBML_LAYOUT_COMPARTMENTGLYPH );
   fail_unless( cg->getMetaId()   == "" );

   fail_unless(cg->getLevel() == level);
   fail_unless(cg->getVersion() == version);

   fail_unless( cg->isSetId() == false );
   fail_unless( cg->getId()          == "" );
   fail_unless( !cg->isSetId());
   fail_unless( !cg->isSetCompartmentId());
   
   delete cg;

   level=2;
   version=3;
   cg=new CompartmentGlyph(level, version); 
   fail_unless( cg->getTypeCode() == SBML_LAYOUT_COMPARTMENTGLYPH );
   fail_unless( cg->getMetaId()   == "" );

   fail_unless(cg->getLevel() == level);
   fail_unless(cg->getVersion() == version);

   fail_unless( cg->isSetId() == false );
   fail_unless( cg->getId()          == "" );
   fail_unless( !cg->isSetId());
   fail_unless( !cg->isSetCompartmentId());
   
   delete cg;
}
END_TEST

START_TEST ( test_CompartmentGlyph_new_WithNamespace )
{
   SBMLNamespaces* ns = new SBMLNamespaces;
   CompartmentGlyph *cg=new CompartmentGlyph(ns); 
   fail_unless( cg->getTypeCode() == SBML_LAYOUT_COMPARTMENTGLYPH );
   fail_unless( cg->getMetaId()   == "" );

   fail_unless(cg->getLevel() == SBML_DEFAULT_LEVEL);
   fail_unless(cg->getVersion() == SBML_DEFAULT_VERSION);

   fail_unless( cg->isSetId() == false );
   fail_unless( cg->getId()          == "" );
   fail_unless( !cg->isSetId());
   fail_unless( !cg->isSetCompartmentId());

   delete cg;
   delete ns;

   ns = new SBMLNamespaces(2,3);
   cg=new CompartmentGlyph(ns); 
   fail_unless( cg->getTypeCode() == SBML_LAYOUT_COMPARTMENTGLYPH );
   fail_unless( cg->getMetaId()   == "" );

   fail_unless(cg->getLevel() == 2);
   fail_unless(cg->getVersion() == 3);

   fail_unless( cg->isSetId() == false );
   fail_unless( cg->getId()          == "" );
   fail_unless( !cg->isSetId());
   fail_unless( !cg->isSetCompartmentId());
   
   delete cg;
   delete ns;
}
END_TEST


START_TEST ( test_CompartmentGlyph_new_with_id_and_compartmentid)
{
    std::string id="TestCompartmentGlyph";
    std::string compId="TestCompartment";
    CompartmentGlyph* cg=new CompartmentGlyph(id,compId);
    fail_unless(cg->isSetCompartmentId());
    fail_unless(cg->getCompartmentId()==compId);
    delete cg;
}
END_TEST

START_TEST ( test_CompartmentGlyph_setCompartmentId )
{
    std::string compId="TestCompartmentGlyph";
    CG->setCompartmentId(compId);
    fail_unless(CG->isSetCompartmentId());
    fail_unless(CG->getCompartmentId()==compId);
    compId="";
    CG->setCompartmentId(compId);
    fail_unless(!CG->isSetCompartmentId());
}
END_TEST

START_TEST ( test_CompartmentGlyph_copyConstructor )
{
    CompartmentGlyph* cg1=new CompartmentGlyph();
    XMLNode* notes=new XMLNode();
    cg1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    cg1->setAnnotation(annotation);
    CompartmentGlyph* cg2=new CompartmentGlyph(*cg1);
    delete cg2;
    delete cg1;
}
END_TEST

START_TEST ( test_CompartmentGlyph_assignmentOperator )
{
    CompartmentGlyph* cg1=new CompartmentGlyph();
    XMLNode* notes=new XMLNode();
    cg1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    cg1->setAnnotation(annotation);
    CompartmentGlyph* cg2=new CompartmentGlyph();
    (*cg2)=(*cg1);
    delete cg2;
    delete cg1;
}
END_TEST

Suite *
create_suite_CompartmentGlyph (void)
{
  Suite *suite = suite_create("CompartmentGlyph");
  TCase *tcase = tcase_create("CompartmentGlyph");

  tcase_add_checked_fixture( tcase,
                             CompartmentGlyphTest_setup,
                             CompartmentGlyphTest_teardown );


  tcase_add_test( tcase, test_CompartmentGlyph_new                               );
  tcase_add_test( tcase, test_CompartmentGlyph_new_WithLevelVersionAndNamespaces );
  tcase_add_test( tcase, test_CompartmentGlyph_new_WithNamespace                 );
  tcase_add_test( tcase, test_CompartmentGlyph_new_with_id_and_compartmentid     );
  tcase_add_test( tcase, test_CompartmentGlyph_setCompartmentId                  );
  tcase_add_test( tcase, test_CompartmentGlyph_copyConstructor                   );
  tcase_add_test( tcase, test_CompartmentGlyph_assignmentOperator                );
  
  suite_add_tcase(suite, tcase);

  return suite;
}



END_C_DECLS
