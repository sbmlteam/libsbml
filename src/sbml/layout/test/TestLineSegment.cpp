/**
 * Filename    : TestLineSegment.cpp
 * Description : Unit tests for LineSegment
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


#include <common/common.h>
#include <common/extern.h>

#include "sbml/SBase.h"
#include "LineSegment.h"
#include "Point.h"

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static LineSegment_t* LS;

void
LineSegmentTest_setup (void)
{
    LS = LineSegment_create();

    if(LS == NULL)
    {
        fail("LineSegment_create(); returned a NULL pointer.");
    }
}

void
LineSegmentTest_teardown (void)
{
    LineSegment_free(LS);
}

START_TEST ( test_LineSegment_create )
{
   fail_unless( SBase_getTypeCode   ((SBase_t*) LS) == SBML_LAYOUT_LINESEGMENT );
   fail_unless( SBase_getMetaId     ((SBase_t*) LS) == NULL );
//   fail_unless( SBase_getNotes      ((SBase_t*) LS) == NULL );
//   fail_unless( SBase_getAnnotation ((SBase_t*) LS) == NULL );

   fail_unless( LineSegment_isSetId(LS) == 0 );
   
   Point_t *pos=LineSegment_getStart(LS);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == 0.0);  
   fail_unless(Point_getYOffset(pos) == 0.0);  
   fail_unless(Point_getZOffset(pos) == 0.0);  
   
   pos=LineSegment_getEnd(LS);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == 0.0);  
   fail_unless(Point_getYOffset(pos) == 0.0);  
   fail_unless(Point_getZOffset(pos) == 0.0);  
   
}
END_TEST

START_TEST ( test_LineSegment_new_WithLevelVersionAndNamespaces )
{
   unsigned int level=1;
   unsigned int version=2;
   LineSegment *ls=new LineSegment(level, version); 
   fail_unless( ls->getTypeCode() == SBML_LAYOUT_LINESEGMENT );
   fail_unless( ls->getMetaId()   == "" );

   fail_unless(ls->getLevel() == level);
   fail_unless(ls->getVersion() == version);

   fail_unless( ls->isSetId() == false );
   Point *pos=ls->getStart();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   pos=ls->getEnd();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
    
   delete ls;

   level=2;
   version=3;
   ls=new LineSegment(level, version); 
   fail_unless( ls->getTypeCode() == SBML_LAYOUT_LINESEGMENT );
   fail_unless( ls->getMetaId()   == "" );

   fail_unless(ls->getLevel() == level);
   fail_unless(ls->getVersion() == version);

   fail_unless( ls->isSetId() == false );
   pos=ls->getStart();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   pos=ls->getEnd();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
    
   delete ls;
}
END_TEST

START_TEST ( test_LineSegment_new_WithNamespace )
{
   SBMLNamespaces* ns=new SBMLNamespaces;
   LineSegment *ls=new LineSegment(ns); 
   fail_unless( ls->getTypeCode() == SBML_LAYOUT_LINESEGMENT );
   fail_unless( ls->getMetaId()   == "" );

   fail_unless(ls->getLevel() == SBML_DEFAULT_LEVEL);
   fail_unless(ls->getVersion() == SBML_DEFAULT_VERSION);

   fail_unless( ls->isSetId() == false );
   Point *pos=ls->getStart();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   pos=ls->getEnd();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
    
   delete ls;
   delete ns;

   ns = new SBMLNamespaces(2,3);
   ls=new LineSegment(ns); 
   fail_unless( ls->getTypeCode() == SBML_LAYOUT_LINESEGMENT );
   fail_unless( ls->getMetaId()   == "" );

   fail_unless(ls->getLevel() == 2);
   fail_unless(ls->getVersion() == 3);

   fail_unless( ls->isSetId() == false );
   pos=ls->getStart();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   pos=ls->getEnd();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
    
   delete ls;
   delete ns;
}
END_TEST


START_TEST ( test_SBase_setId )
{
    const char* id="LineSegment";
    LineSegment_setId(LS,id);
    fail_unless(LineSegment_isSetId(LS) != 0 );
    fail_unless(strncmp(id,LineSegment_getId(LS),strlen(id)) == 0);
}
END_TEST

START_TEST ( test_SBase_setId_NULL )
{
    LineSegment_setId(LS,NULL);
    fail_unless(LineSegment_isSetId(LS) == 0 );
    fail_unless(LineSegment_getId(LS) == NULL);
}
END_TEST


START_TEST ( test_LineSegment_createWithPoints )
{
   Point_t *start=Point_createWithCoordinates(1.1,-2.2,3.3); 
   Point_t *end  =Point_createWithCoordinates(-4.4,5.5,-6.6);

   LineSegment_t *ls=LineSegment_createWithPoints(start,end);
   
   fail_unless( SBase_getTypeCode   ((SBase_t*) ls) == SBML_LAYOUT_LINESEGMENT );
   fail_unless( SBase_getMetaId     ((SBase_t*) ls) == NULL );
//   fail_unless( SBase_getNotes      ((SBase_t*) ls) == NULL );
//   fail_unless( SBase_getAnnotation ((SBase_t*) ls) == NULL );

   fail_unless( LineSegment_isSetId(ls) == 0 );
   
   Point_t *pos=LineSegment_getStart(ls);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == Point_getXOffset(start));  
   fail_unless(Point_getYOffset(pos) == Point_getYOffset(start));  
   fail_unless(Point_getZOffset(pos) == Point_getZOffset(start));  
   
   pos=LineSegment_getEnd(ls);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == Point_getXOffset(end));  
   fail_unless(Point_getYOffset(pos) == Point_getYOffset(end));  
   fail_unless(Point_getZOffset(pos) == Point_getZOffset(end));  

   Point_free(start);
   Point_free(end);

   LineSegment_free(ls);
  
}
END_TEST

START_TEST ( test_LineSegment_createWithPoints_NULL )
{
   LineSegment_t *ls=LineSegment_createWithPoints(NULL,NULL);
   
   fail_unless( SBase_getTypeCode   ((SBase_t*) ls) == SBML_LAYOUT_LINESEGMENT );
   fail_unless( SBase_getMetaId     ((SBase_t*) ls) == NULL );
//   fail_unless( SBase_getNotes      ((SBase_t*) ls) == NULL );
//   fail_unless( SBase_getAnnotation ((SBase_t*) ls) == NULL );

   fail_unless( LineSegment_isSetId(ls) == 0 );
   
   Point_t *pos=LineSegment_getStart(ls);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == 0.0);  
   fail_unless(Point_getYOffset(pos) == 0.0);  
   fail_unless(Point_getZOffset(pos) == 0.0);  
   
   pos=LineSegment_getEnd(ls);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == 0.0);  
   fail_unless(Point_getYOffset(pos) == 0.0);  
   fail_unless(Point_getZOffset(pos) == 0.0);  
 
   LineSegment_free(ls);
}
END_TEST

START_TEST ( test_LineSegment_createWithCoordinates )
{
   LineSegment_t* ls=LineSegment_createWithCoordinates(1.1,-2.2,3.3,-4.4,5.5,-6.6);
    
   fail_unless( SBase_getTypeCode   ((SBase_t*) ls) == SBML_LAYOUT_LINESEGMENT );
   fail_unless( SBase_getMetaId     ((SBase_t*) ls) == NULL );
//   fail_unless( SBase_getNotes      ((SBase_t*) ls) == NULL );
//   fail_unless( SBase_getAnnotation ((SBase_t*) ls) == NULL );

   fail_unless( LineSegment_isSetId(ls) == 0 );
   
   Point_t *pos=LineSegment_getStart(ls);
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() ==  1.1);  
   fail_unless(pos->getYOffset() == -2.2);  
   fail_unless(pos->getZOffset() ==  3.3);  
   
   pos=LineSegment_getEnd(ls);
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == -4.4);  
   fail_unless(pos->getYOffset() ==  5.5);  
   fail_unless(pos->getZOffset() == -6.6);  

   LineSegment_free(ls); 
}
END_TEST

START_TEST (test_LineSegment_free_NULL)
{
    LineSegment_free(NULL);
}
END_TEST

START_TEST (test_LineSegment_setStart){
    Point_t *pos=Point_createWithCoordinates(1.1,-2.2,3.3);
    LineSegment_setStart(LS,pos);

    Point_t* POS=LineSegment_getStart(LS);

    fail_unless(Point_getXOffset(pos) == Point_getXOffset(POS));
    fail_unless(Point_getYOffset(pos) == Point_getYOffset(POS));
    fail_unless(Point_getZOffset(pos) == Point_getZOffset(POS)); 
    Point_free(pos);
}
END_TEST

START_TEST (test_LineSegment_setEnd ){
    Point_t *pos=Point_createWithCoordinates(-4.4,5.5,-6.6);
    LineSegment_setEnd(LS,pos);

    Point_t* POS=LineSegment_getEnd(LS);

    fail_unless(Point_getXOffset(pos) == Point_getXOffset(POS));
    fail_unless(Point_getYOffset(pos) == Point_getYOffset(POS));
    fail_unless(Point_getZOffset(pos) == Point_getZOffset(POS));
    Point_free(pos);
}
END_TEST

START_TEST ( test_LineSegment_createFrom )
{
   Point_t* start=Point_createWithCoordinates(1.1,-2.2,3.3); 
   Point_t* end=Point_createWithCoordinates(-4.4,5.5,-6.6); 
   LineSegment_setStart(LS,start);
   LineSegment_setEnd(LS,end);
   LineSegment_setId(LS,"TestID");
   LineSegment_t* ls=LineSegment_createFrom(LS);
   
   
   fail_unless( SBase_getTypeCode   ((SBase_t*) ls) == SBML_LAYOUT_LINESEGMENT );
   const char* c1=SBase_getMetaId((SBase_t*)LS);
   const char* c2=SBase_getMetaId((SBase_t*)ls);
   
   if(SBase_isSetMetaId((SBase_t*)LS)){
     fail_unless( strncmp(c1 , c2 ,strlen( c1 )+1 ) );
   }
   else
   {
     fail_unless(!(c1 || c2));
   }
//   c1=SBase_getNotes((SBase_t*)LS);
//   c2=SBase_getNotes((SBase_t*)ls);
//   
//   if(SBase_isSetNotes((SBase_t*)LS))
//   {
//     fail_unless( strncmp(c1 , c2 ,strlen( c1 ) + 1 ) );
//   }
//   else
//   {
//     fail_unless(!(c1 || c2));
//   }
//   
//   c1=SBase_getAnnotation((SBase_t*)LS);
//   c2=SBase_getAnnotation((SBase_t*)ls);
//   
//   if(SBase_isSetAnnotation((SBase_t*)LS))
//   {
//     fail_unless( strncmp(c1 , c2 ,strlen( c1 ) + 1) );
//   }
//   else
//   {
//     fail_unless(!(c1 || c2));
//   }
   
   fail_unless( LineSegment_isSetId(ls) == LineSegment_isSetId(LS) );
   if( LineSegment_isSetId(ls) )
   {
       c1=LineSegment_getId(LS);
       c2=LineSegment_getId(ls);
       fail_unless( strncmp(c1 , c2 ,strlen( c1 ) + 1 ) == 0);
     
   }
   
   Point_t *pos=LineSegment_getStart(ls);
   Point_t *POS=LineSegment_getStart(LS);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == Point_getXOffset(POS));  
   fail_unless(Point_getYOffset(pos) == Point_getYOffset(POS));  
   fail_unless(Point_getZOffset(pos) == Point_getZOffset(POS));  
   
   pos=LineSegment_getEnd(ls);
   POS=LineSegment_getEnd(LS);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == Point_getXOffset(POS));  
   fail_unless(Point_getYOffset(pos) == Point_getYOffset(POS));  
   fail_unless(Point_getZOffset(pos) == Point_getZOffset(POS));  
 
   Point_free(start);
   Point_free(end);
   LineSegment_free(ls);
}
END_TEST

START_TEST (test_LineSegment_setStart_NULL )
{
    LineSegment_setStart(LS,NULL);

    Point_t *pos=LineSegment_getStart(LS);
    fail_unless(pos != NULL);
    fail_unless(Point_getXOffset(pos) == 0.0);
    fail_unless(Point_getYOffset(pos) == 0.0);
    fail_unless(Point_getZOffset(pos) == 0.0);
}
END_TEST

START_TEST (test_LineSegment_setEnd_NULL )
{
    LineSegment_setEnd(LS,NULL);
    Point_t *pos=LineSegment_getEnd(LS);
    fail_unless(pos != NULL);
    fail_unless(Point_getXOffset(pos) == 0.0);
    fail_unless(Point_getYOffset(pos) == 0.0);
    fail_unless(Point_getZOffset(pos) == 0.0);
}
END_TEST

START_TEST ( test_LineSegment_copyConstructor )
{
    LineSegment* ls1=new LineSegment();
    XMLNode* notes=new XMLNode();
    ls1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    ls1->setAnnotation(annotation);
    LineSegment* ls2=new LineSegment(*ls1);
    delete ls2;
    delete ls1;
}
END_TEST

START_TEST ( test_LineSegment_assignmentOperator )
{
    LineSegment* ls1=new LineSegment();
    XMLNode* notes=new XMLNode();
    ls1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    ls1->setAnnotation(annotation);
    LineSegment* ls2=new LineSegment();
    (*ls2)=(*ls1);
    delete ls2;
    delete ls1;
}
END_TEST

   
Suite *
create_suite_LineSegment (void)
{
  Suite *suite = suite_create("LineSegment");
  TCase *tcase = tcase_create("LineSegment");


  tcase_add_checked_fixture( tcase,
                             LineSegmentTest_setup,
                             LineSegmentTest_teardown );

  tcase_add_test( tcase, test_LineSegment_create                            );
  tcase_add_test( tcase, test_LineSegment_new_WithLevelVersionAndNamespaces );
  tcase_add_test( tcase, test_LineSegment_new_WithNamespace                 );
  tcase_add_test( tcase, test_SBase_setId                                   );
  tcase_add_test( tcase, test_SBase_setId_NULL                              );
  tcase_add_test( tcase, test_LineSegment_createWithPoints                  );
  tcase_add_test( tcase, test_LineSegment_createWithPoints_NULL             );
  tcase_add_test( tcase, test_LineSegment_createWithCoordinates             );
  tcase_add_test( tcase, test_LineSegment_free_NULL                         );
  tcase_add_test( tcase, test_LineSegment_setStart                          );
  tcase_add_test( tcase, test_LineSegment_setStart_NULL                     );
  tcase_add_test( tcase, test_LineSegment_setEnd                            );
  tcase_add_test( tcase, test_LineSegment_setEnd_NULL                       );
  tcase_add_test( tcase, test_LineSegment_createFrom                        );
  tcase_add_test( tcase, test_LineSegment_copyConstructor                   );
  tcase_add_test( tcase, test_LineSegment_assignmentOperator                );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
