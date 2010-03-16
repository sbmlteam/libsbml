/**
 * Filename    : TestCubicBezier.cpp
 * Description : Unit tests for CubicBezier
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
#include "CubicBezier.h"
#include "Point.h"

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static CubicBezier_t* CB;

void
CubicBezierTest_setup (void)
{
    CB = CubicBezier_create();

    if(CB == NULL)
    {
        fail("CubicBezier_create(); returned a NULL pointer.");
    }
}

void
CubicBezierTest_teardown (void)
{
    CubicBezier_free(CB);
}

START_TEST ( test_CubicBezier_create )
{
   fail_unless( SBase_getTypeCode   ((SBase_t*) CB) == SBML_LAYOUT_CUBICBEZIER );
   fail_unless( SBase_getMetaId     ((SBase_t*) CB) == NULL );
//   fail_unless( SBase_getNotes      ((SBase_t*) CB) == NULL );
//   fail_unless( SBase_getAnnotation ((SBase_t*) CB) == NULL );

   fail_unless( LineSegment_isSetId((LineSegment_t*)CB) == 0 );
   
   Point_t *pos=CubicBezier_getStart(CB);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == 0.0);  
   fail_unless(Point_getYOffset(pos) == 0.0);  
   fail_unless(Point_getZOffset(pos) == 0.0);  
   
   pos=CubicBezier_getBasePoint1(CB);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == 0.0);  
   fail_unless(Point_getYOffset(pos) == 0.0);  
   fail_unless(Point_getZOffset(pos) == 0.0);  

   pos=CubicBezier_getBasePoint2(CB);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == 0.0);  
   fail_unless(Point_getYOffset(pos) == 0.0);  
   fail_unless(Point_getZOffset(pos) == 0.0);  
   
   pos=CubicBezier_getEnd(CB);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == 0.0);  
   fail_unless(Point_getYOffset(pos) == 0.0);  
   fail_unless(Point_getZOffset(pos) == 0.0);  
   
}
END_TEST

START_TEST ( test_CubicBezier_new_WithLevelVersionAndNamespaces )
{
   unsigned int level=1;
   unsigned int version=2;
   CubicBezier *cb=new CubicBezier(level, version); 
   fail_unless( cb->getTypeCode() == SBML_LAYOUT_CUBICBEZIER );
   fail_unless( cb->getMetaId()   == "" );

   fail_unless(cb->getLevel() == level);
   fail_unless(cb->getVersion() == version);

   fail_unless( cb->isSetId() == false );
   
   Point *pos=cb->getStart();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   pos=cb->getBasePoint1();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  

   pos=cb->getBasePoint2();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   pos=cb->getEnd();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
    
   delete cb;

   level=2;
   version=3;
   cb=new CubicBezier(level, version); 
   fail_unless( cb->getTypeCode() == SBML_LAYOUT_CUBICBEZIER );
   fail_unless( cb->getMetaId()   == "" );

   fail_unless(cb->getLevel() == level);
   fail_unless(cb->getVersion() == version);

   fail_unless( cb->isSetId() == false );
   
   pos=cb->getStart();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   pos=cb->getBasePoint1();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  

   pos=cb->getBasePoint2();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   pos=cb->getEnd();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
    
   delete cb;
}
END_TEST

START_TEST ( test_CubicBezier_new_WithNamespace )
{
   SBMLNamespaces* ns = new SBMLNamespaces;
   CubicBezier *cb=new CubicBezier(ns); 
   fail_unless( cb->getTypeCode() == SBML_LAYOUT_CUBICBEZIER );
   fail_unless( cb->getMetaId()   == "" );

   fail_unless(cb->getLevel() == SBML_DEFAULT_LEVEL);
   fail_unless(cb->getVersion() == SBML_DEFAULT_VERSION);

   fail_unless( cb->isSetId() == false );
   Point *pos=cb->getStart();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   pos=cb->getBasePoint1();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  

   pos=cb->getBasePoint2();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   pos=cb->getEnd();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
    
   delete cb;
   delete ns;

   ns = new SBMLNamespaces(2,3);
   cb=new CubicBezier(ns); 
   fail_unless( cb->getTypeCode() == SBML_LAYOUT_CUBICBEZIER );
   fail_unless( cb->getMetaId()   == "" );

   fail_unless(cb->getLevel() == 2);
   fail_unless(cb->getVersion() == 3);

   fail_unless( cb->isSetId() == false );
   pos=cb->getStart();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   pos=cb->getBasePoint1();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  

   pos=cb->getBasePoint2();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   pos=cb->getEnd();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
    
   delete cb;
   delete ns;
}
END_TEST


START_TEST ( test_CubicBezier_createWithPoints )
{
   Point_t *start=Point_createWithCoordinates(1.1,-2.2,3.3); 
   Point_t *base1=Point_createWithCoordinates(-0.5,2.4,5.6); 
   Point_t *base2=Point_createWithCoordinates(7.8,-0.3,-1.2); 
   Point_t *end  =Point_createWithCoordinates(-4.4,5.5,-6.6);

   CubicBezier_t *cb=CubicBezier_createWithPoints(start,base1,base2,end);
   
   fail_unless( SBase_getTypeCode   ((SBase_t*) cb) == SBML_LAYOUT_CUBICBEZIER );
   fail_unless( SBase_getMetaId     ((SBase_t*) cb) == NULL );
//   fail_unless( SBase_getNotes      ((SBase_t*) cb) == NULL );
//   fail_unless( SBase_getAnnotation ((SBase_t*) cb) == NULL );

   fail_unless( CubicBezier_isSetId(cb) == 0 );
   
   Point_t *pos=CubicBezier_getStart(cb);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == Point_getXOffset(start));  
   fail_unless(Point_getYOffset(pos) == Point_getYOffset(start));  
   fail_unless(Point_getZOffset(pos) == Point_getZOffset(start));  
   
   pos=CubicBezier_getBasePoint1(cb);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == Point_getXOffset(base1));  
   fail_unless(Point_getYOffset(pos) == Point_getYOffset(base1));  
   fail_unless(Point_getZOffset(pos) == Point_getZOffset(base1));  

   pos=CubicBezier_getBasePoint2(cb);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == Point_getXOffset(base2));  
   fail_unless(Point_getYOffset(pos) == Point_getYOffset(base2));  
   fail_unless(Point_getZOffset(pos) == Point_getZOffset(base2));  
   
   pos=CubicBezier_getEnd(cb);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == Point_getXOffset(end));  
   fail_unless(Point_getYOffset(pos) == Point_getYOffset(end));  
   fail_unless(Point_getZOffset(pos) == Point_getZOffset(end));  

    Point_free(start);
    Point_free(base1);
    Point_free(base2);
    Point_free(end);
    CubicBezier_free(cb);
}
END_TEST

START_TEST ( test_CubicBezier_createWithPoints_NULL )
{
   CubicBezier_t *cb=CubicBezier_createWithPoints(NULL,NULL,NULL,NULL);
   
   fail_unless( SBase_getTypeCode   ((SBase_t*) cb) == SBML_LAYOUT_CUBICBEZIER );
   fail_unless( SBase_getMetaId     ((SBase_t*) cb) == NULL );
//   fail_unless( SBase_getNotes      ((SBase_t*) cb) == NULL );
//   fail_unless( SBase_getAnnotation ((SBase_t*) cb) == NULL );

   fail_unless( LineSegment_isSetId((LineSegment_t*)cb) == 0 );
   
   Point_t *pos=CubicBezier_getStart(cb);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == 0.0);  
   fail_unless(Point_getYOffset(pos) == 0.0);  
   fail_unless(Point_getZOffset(pos) == 0.0);  
   
   pos=CubicBezier_getBasePoint1(cb);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == 0.0);  
   fail_unless(Point_getYOffset(pos) == 0.0);  
   fail_unless(Point_getZOffset(pos) == 0.0);  

   pos=CubicBezier_getBasePoint2(cb);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == 0.0);  
   fail_unless(Point_getYOffset(pos) == 0.0);  
   fail_unless(Point_getZOffset(pos) == 0.0);  
   
   pos=CubicBezier_getEnd(cb);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == 0.0);  
   fail_unless(Point_getYOffset(pos) == 0.0);  
   fail_unless(Point_getZOffset(pos) == 0.0);  
 
   CubicBezier_free(cb);
}
END_TEST

START_TEST ( test_CubicBezier_createWithCoordinates )
{
   CubicBezier_t* cb=CubicBezier_createWithCoordinates(1.1,-2.2,3.3,
                                                      -4.4,5.5,-6.6,
                                                      7.7,-8.8,9.9,
                                                      -10.10,11.11,-12.12);
    
   fail_unless( SBase_getTypeCode   ((SBase_t*) cb) == SBML_LAYOUT_CUBICBEZIER );
   fail_unless( SBase_getMetaId     ((SBase_t*) cb) == NULL );
//   fail_unless( SBase_getNotes      ((SBase_t*) cb) == NULL );
//   fail_unless( SBase_getAnnotation ((SBase_t*) cb) == NULL );

   fail_unless( LineSegment_isSetId((LineSegment_t*)cb) == 0 );
   
   Point_t *pos=CubicBezier_getStart(cb);
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() ==  1.1);  
   fail_unless(pos->getYOffset() == -2.2);  
   fail_unless(pos->getZOffset() ==  3.3);  

   pos=CubicBezier_getBasePoint1(cb);
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == -4.4);  
   fail_unless(pos->getYOffset() ==  5.5);  
   fail_unless(pos->getZOffset() == -6.6);  
   
   pos=CubicBezier_getBasePoint2(cb);
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() ==  7.7);  
   fail_unless(pos->getYOffset() == -8.8);  
   fail_unless(pos->getZOffset() ==  9.9);  
    
   pos=CubicBezier_getEnd(cb);
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == -10.10);  
   fail_unless(pos->getYOffset() ==  11.11);  
   fail_unless(pos->getZOffset() == -12.12);  

   CubicBezier_free(cb); 
}
END_TEST

START_TEST (test_CubicBezier_free_NULL)
{
    CubicBezier_free(NULL);
}
END_TEST

START_TEST (test_CubicBezier_setStart){
    Point_t *pos=Point_createWithCoordinates(1.1,-2.2,3.3);
    CubicBezier_setStart(CB,pos);

    Point_t* POS=CubicBezier_getStart(CB);

    fail_unless(Point_getXOffset(pos) == Point_getXOffset(POS));
    fail_unless(Point_getYOffset(pos) == Point_getYOffset(POS));
    fail_unless(Point_getZOffset(pos) == Point_getZOffset(POS)); 
    Point_free(pos);

}
END_TEST

START_TEST (test_CubicBezier_setBasePoint1 ){
    Point_t *pos=Point_createWithCoordinates(7.7,-8.8,9.9);
    CubicBezier_setBasePoint1(CB,pos);

    Point_t* POS=CubicBezier_getBasePoint1(CB);

    fail_unless(Point_getXOffset(pos) == Point_getXOffset(POS));
    fail_unless(Point_getYOffset(pos) == Point_getYOffset(POS));
    fail_unless(Point_getZOffset(pos) == Point_getZOffset(POS));
    Point_free(pos);
}
END_TEST

START_TEST (test_CubicBezier_setBasePoint2 ){
    Point_t *pos=Point_createWithCoordinates(-10.10,11.11,-12.12);
    CubicBezier_setBasePoint2(CB,pos);

    Point_t* POS=CubicBezier_getBasePoint2(CB);

    fail_unless(Point_getXOffset(pos) == Point_getXOffset(POS));
    fail_unless(Point_getYOffset(pos) == Point_getYOffset(POS));
    fail_unless(Point_getZOffset(pos) == Point_getZOffset(POS));

    Point_free(pos);
}
END_TEST

START_TEST (test_CubicBezier_setEnd ){
    Point_t *pos=Point_createWithCoordinates(-4.4,5.5,-6.6);
    CubicBezier_setEnd(CB,pos);

    Point_t* POS=CubicBezier_getEnd(CB);

    fail_unless(Point_getXOffset(pos) == Point_getXOffset(POS));
    fail_unless(Point_getYOffset(pos) == Point_getYOffset(POS));
    fail_unless(Point_getZOffset(pos) == Point_getZOffset(POS));
    Point_free(pos);
}
END_TEST

START_TEST ( test_CubicBezier_createFrom )
{
   Point_t* start=Point_createWithCoordinates(1.1,-2.2,3.3);
   Point_t* base1=Point_createWithCoordinates(-4.4,5.5,-6.6);
   Point_t* base2=Point_createWithCoordinates(7.7,-8.8,9.9);
   Point_t* end=Point_createWithCoordinates(-10.10,11.11,-12.12);
   CubicBezier_setStart(CB,start);
   CubicBezier_setBasePoint1(CB,base1);
   CubicBezier_setBasePoint2(CB,base2);
   CubicBezier_setEnd(CB,end);
   CubicBezier_t* cb=CubicBezier_createFrom(CB);
   fail_unless( SBase_getTypeCode   ((SBase_t*) cb) == SBML_LAYOUT_CUBICBEZIER );
   const char* c1=SBase_getMetaId((SBase_t*)CB);
   const char* c2=SBase_getMetaId((SBase_t*)cb);
   if(SBase_isSetMetaId((SBase_t*)CB))
   {
       fail_unless( strncmp(c1 , c2 ,strlen( c1)+1 )==0 );
   }
   else{
       fail_unless(!(c1 || c2));
   }

//   c1=SBase_getNotes((SBase_t*)CB);
//   c2=SBase_getNotes((SBase_t*)cb);
//   if(SBase_isSetNotes((SBase_t*)CB))
//   {
//       fail_unless( strncmp(c1 , c2 ,strlen( c1)+1 )==0 );
//   }
//   else{
//       fail_unless(!(c1 || c2));
//   }

//   c1=SBase_getAnnotation((SBase_t*)CB);
//   c2=SBase_getAnnotation((SBase_t*)cb);
//   if(SBase_isSetAnnotation((SBase_t*)CB))
//   {
//       fail_unless( strncmp(c1 , c2 ,strlen( c1)+1 )==0 );
//   }
//   else{
//       fail_unless(!(c1 || c2));
//   }

   fail_unless( LineSegment_isSetId((LineSegment_t*)cb) == LineSegment_isSetId((LineSegment_t*)CB) );
   if( LineSegment_isSetId((LineSegment_t*)cb) )
   {
       c1=LineSegment_getId((LineSegment_t*)CB);
       c2=LineSegment_getId((LineSegment_t*)cb);
       if(LineSegment_isSetId((LineSegment_t*)CB))
       {
           fail_unless( strncmp(c1 , c2 ,strlen( c1)+1 )==0 );
       }
       else{
         fail_unless(!(c1 || c2));
       }
   }
   
   Point_t *pos=CubicBezier_getStart(cb);
   Point_t *POS=CubicBezier_getStart(CB);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == Point_getXOffset(POS));  
   fail_unless(Point_getYOffset(pos) == Point_getYOffset(POS));  
   fail_unless(Point_getZOffset(pos) == Point_getZOffset(POS));  

   pos=CubicBezier_getBasePoint1(cb);
   POS=CubicBezier_getBasePoint1(CB);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == Point_getXOffset(POS));  
   fail_unless(Point_getYOffset(pos) == Point_getYOffset(POS));  
   fail_unless(Point_getZOffset(pos) == Point_getZOffset(POS));  
   
   pos=CubicBezier_getBasePoint2(cb);
   POS=CubicBezier_getBasePoint2(CB);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == Point_getXOffset(POS));  
   fail_unless(Point_getYOffset(pos) == Point_getYOffset(POS));  
   fail_unless(Point_getZOffset(pos) == Point_getZOffset(POS));  
    
   pos=CubicBezier_getEnd(cb);
   POS=CubicBezier_getEnd(CB);
   fail_unless(pos != NULL);
   fail_unless(Point_getXOffset(pos) == Point_getXOffset(POS));  
   fail_unless(Point_getYOffset(pos) == Point_getYOffset(POS));  
   fail_unless(Point_getZOffset(pos) == Point_getZOffset(POS));  
   Point_free(start);
   Point_free(base1);
   Point_free(base2);
   Point_free(end);

  CubicBezier_free(cb); 
}
END_TEST

START_TEST (test_CubicBezier_setStart_NULL )
{
    CubicBezier_setStart(CB,NULL);

    Point_t *pos=CubicBezier_getStart(CB);
    fail_unless(pos != NULL);
    fail_unless(Point_getXOffset(pos) == 0.0);
    fail_unless(Point_getYOffset(pos) == 0.0);
    fail_unless(Point_getZOffset(pos) == 0.0);
}
END_TEST

START_TEST (test_CubicBezier_setBasePoint1_NULL )
{
    CubicBezier_setBasePoint1(CB,NULL);

    Point_t *pos=CubicBezier_getBasePoint1(CB);
    fail_unless(pos != NULL);
    fail_unless(Point_getXOffset(pos) == 0.0);
    fail_unless(Point_getYOffset(pos) == 0.0);
    fail_unless(Point_getZOffset(pos) == 0.0);
}
END_TEST

START_TEST (test_CubicBezier_setBasePoint2_NULL )
{
    CubicBezier_setBasePoint2(CB,NULL);

    Point_t *pos=CubicBezier_getBasePoint2(CB);
    fail_unless(pos != NULL);
    fail_unless(Point_getXOffset(pos) == 0.0);
    fail_unless(Point_getYOffset(pos) == 0.0);
    fail_unless(Point_getZOffset(pos) == 0.0);
}
END_TEST


START_TEST (test_CubicBezier_setEnd_NULL )
{
    CubicBezier_setEnd(CB,NULL);
    Point_t *pos=CubicBezier_getEnd(CB);
    fail_unless(pos != NULL);
    fail_unless(Point_getXOffset(pos) == 0.0);
    fail_unless(Point_getYOffset(pos) == 0.0);
    fail_unless(Point_getZOffset(pos) == 0.0);
}
END_TEST

START_TEST ( test_CubicBezier_copyConstructor )
{
    CubicBezier* cb1=new CubicBezier();
    XMLNode* notes=new XMLNode();
    cb1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    cb1->setAnnotation(annotation);
    CubicBezier* cb2=new CubicBezier(*cb1);
    delete cb2;
    delete cb1;
}
END_TEST

START_TEST ( test_CubicBezier_assignmentOperator )
{
    CubicBezier* cb1=new CubicBezier();
    XMLNode* notes=new XMLNode();
    cb1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    cb1->setAnnotation(annotation);
    CubicBezier* cb2=new CubicBezier();
    (*cb2)=(*cb1);
    delete cb2;
    delete cb1;
}
END_TEST

Suite *
create_suite_CubicBezier (void)
{
  Suite *suite = suite_create("CubicBezier");
  TCase *tcase = tcase_create("CubicBezier");


  tcase_add_checked_fixture( tcase,
                             CubicBezierTest_setup,
                             CubicBezierTest_teardown );

  tcase_add_test( tcase, test_CubicBezier_create                            );
  tcase_add_test( tcase, test_CubicBezier_new_WithLevelVersionAndNamespaces );
  tcase_add_test( tcase, test_CubicBezier_new_WithNamespace                 );
  tcase_add_test( tcase, test_CubicBezier_createWithPoints                  );
  tcase_add_test( tcase, test_CubicBezier_createWithPoints_NULL             );
  tcase_add_test( tcase, test_CubicBezier_createWithCoordinates             );
  tcase_add_test( tcase, test_CubicBezier_free_NULL                         );
  tcase_add_test( tcase, test_CubicBezier_setStart                          );
  tcase_add_test( tcase, test_CubicBezier_setStart_NULL                     );
  tcase_add_test( tcase, test_CubicBezier_setBasePoint1                     );
  tcase_add_test( tcase, test_CubicBezier_setBasePoint1_NULL                );
  tcase_add_test( tcase, test_CubicBezier_setBasePoint2                     );
  tcase_add_test( tcase, test_CubicBezier_setBasePoint2_NULL                );
  tcase_add_test( tcase, test_CubicBezier_setEnd                            );
  tcase_add_test( tcase, test_CubicBezier_setEnd_NULL                       );
  tcase_add_test( tcase, test_CubicBezier_createFrom                        );
  tcase_add_test( tcase, test_CubicBezier_copyConstructor                   );
  tcase_add_test( tcase, test_CubicBezier_assignmentOperator                );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
