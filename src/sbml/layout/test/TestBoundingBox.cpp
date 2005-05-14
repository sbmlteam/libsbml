/**
 * Filename    : TestBoundingBox.cpp
 * Description : Unit tests for BoundingBox
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

#include <check.h>

#include <common/common.h>
#include <common/extern.h>

#include "sbml/SBase.h"
#include "BoundingBox.h"
#include "Point.h"
#include "Dimensions.h"

BEGIN_C_DECLS

static BoundingBox_t* BB;

void
BoundingBoxTest_setup (void)
{
    BB = BoundingBox_create();

    if(BB == NULL)
    {
        fail("BoundingBox_create(); returned a NULL pointer.");
    }
}

void
BoundingBoxTest_teardown (void)
{
    BoundingBox_free(BB);
}

START_TEST ( test_BoundingBox_create )
{
   fail_unless( SBase_getTypeCode   ((SBase_t*) BB) == SBML_LAYOUT_BOUNDINGBOX );
   fail_unless( SBase_getMetaId     ((SBase_t*) BB) == NULL );
   fail_unless( SBase_getNotes      ((SBase_t*) BB) == NULL );
   fail_unless( SBase_getAnnotation ((SBase_t*) BB) == NULL );

   fail_unless( BoundingBox_isSetId(BB) == 0 );
   
   Point_t *pos=BoundingBox_getPosition(BB);
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   Dimensions_t *dim=BoundingBox_getDimensions(BB); 
   fail_unless(dim != NULL);
   fail_unless(dim->getWidth () == 0.0);  
   fail_unless(dim->getHeight() == 0.0);  
   fail_unless(dim->getDepth () == 0.0);  
 
}
END_TEST


START_TEST ( test_BoundingBox_createWith )
{
   const char* id="BoundingBox"; 
   BoundingBox_t *bb=BoundingBox_createWith(id); 
   fail_unless( SBase_getTypeCode   ((SBase_t*) bb) == SBML_LAYOUT_BOUNDINGBOX );
   fail_unless( SBase_getMetaId     ((SBase_t*) bb) == NULL );
   fail_unless( SBase_getNotes      ((SBase_t*) bb) == NULL );
   fail_unless( SBase_getAnnotation ((SBase_t*) bb) == NULL );

   fail_unless( BoundingBox_isSetId(bb) != 0 );
   fail_unless( strncmp(BoundingBox_getId(bb),id,strlen(id)+1) == 0);
   
   Point_t *pos=BoundingBox_getPosition(bb);
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   Dimensions_t *dim=BoundingBox_getDimensions(bb); 
   fail_unless(dim != NULL);
   fail_unless(dim->getWidth () == 0.0);  
   fail_unless(dim->getHeight() == 0.0);  
   fail_unless(dim->getDepth () == 0.0);  
   
}
END_TEST

START_TEST ( test_BoundingBox_createWith_NULL )
{
   BoundingBox_t *bb=BoundingBox_createWith(NULL); 
   fail_unless( SBase_getTypeCode   ((SBase_t*) bb) == SBML_LAYOUT_BOUNDINGBOX );
   fail_unless( SBase_getMetaId     ((SBase_t*) bb) == NULL );
   fail_unless( SBase_getNotes      ((SBase_t*) bb) == NULL );
   fail_unless( SBase_getAnnotation ((SBase_t*) bb) == NULL );

   fail_unless( BoundingBox_isSetId(bb) == 0 );
   
   Point_t *pos=BoundingBox_getPosition(bb);
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   Dimensions_t *dim=BoundingBox_getDimensions(bb); 
   fail_unless(dim != NULL);
   fail_unless(dim->getWidth () == 0.0);  
   fail_unless(dim->getHeight() == 0.0);  
   fail_unless(dim->getDepth () == 0.0);  
 }
END_TEST

START_TEST ( test_BoundingBox_createWithCoordinates )
{
   const char* id="BoundingBox";
   BoundingBox_t *bb=BoundingBox_createWithCoordinates(id,1.1,-2.2,3.3,-4.4,5.5,-6.6);
   fail_unless( SBase_getTypeCode   ((SBase_t*) bb) == SBML_LAYOUT_BOUNDINGBOX );
   fail_unless( SBase_getMetaId     ((SBase_t*) bb) == NULL );
   fail_unless( SBase_getNotes      ((SBase_t*) bb) == NULL );
   fail_unless( SBase_getAnnotation ((SBase_t*) bb) == NULL );

   fail_unless( BoundingBox_isSetId(bb) != 0 );
   fail_unless( strncmp(BoundingBox_getId(bb),id,strlen(id)+1) == 0);
   
   Point_t *pos=BoundingBox_getPosition(bb);
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() ==  1.1);  
   fail_unless(pos->getYOffset() == -2.2);  
   fail_unless(pos->getZOffset() ==  3.3);  
   
   Dimensions_t *dim=BoundingBox_getDimensions(bb); 
   fail_unless(dim != NULL);
   fail_unless(dim->getWidth () == -4.4);  
   fail_unless(dim->getHeight() ==  5.5);  
   fail_unless(dim->getDepth () == -6.6);  
 }
END_TEST

START_TEST ( test_BoundingBox_createWithCoordinates_NULL )
{
   BoundingBox_t *bb=BoundingBox_createWithCoordinates(NULL,1.1,-2.2,3.3,-4.4,5.5,-6.6);
   fail_unless( SBase_getTypeCode   ((SBase_t*) bb) == SBML_LAYOUT_BOUNDINGBOX );
   fail_unless( SBase_getMetaId     ((SBase_t*) bb) == NULL );
   fail_unless( SBase_getNotes      ((SBase_t*) bb) == NULL );
   fail_unless( SBase_getAnnotation ((SBase_t*) bb) == NULL );

   fail_unless( BoundingBox_isSetId(bb) == 0 );
   
   Point_t *pos=BoundingBox_getPosition(bb);
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() ==  1.1);  
   fail_unless(pos->getYOffset() == -2.2);  
   fail_unless(pos->getZOffset() ==  3.3);  
   
   Dimensions_t *dim=BoundingBox_getDimensions(bb); 
   fail_unless(dim != NULL);
   fail_unless(dim->getWidth () == -4.4);  
   fail_unless(dim->getHeight() ==  5.5);  
   fail_unless(dim->getDepth () == -6.6);  
}
END_TEST

START_TEST ( test_BoundingBox_free_NULL )
{
    BoundingBox_free(NULL);
}
END_TEST

START_TEST ( test_BoundingBox_setId )
{
    const char* id="BoundingBox";
    BoundingBox_setId(BB,id);
    fail_unless(BoundingBox_isSetId(BB) != 0);
    fail_unless(strncmp(BoundingBox_getId(BB),id,strlen(id)+1)==0);
}
END_TEST

START_TEST ( test_BoundingBox_setId_NULL )
{
    BoundingBox_setId(BB,NULL);
    fail_unless(BoundingBox_isSetId(BB) == 0);
    fail_unless(BoundingBox_getId(BB) == NULL);
}
END_TEST

START_TEST ( test_BoundingBox_setPosition )
{
    Point_t *pos=Point_createWithCoordinates(-1.1,2.2,-3.3);
    BoundingBox_setPosition(BB,pos);
    Point_t *pos2=BoundingBox_getPosition(BB);
    fail_unless(pos2 != NULL);
    fail_unless(Point_getXOffset(pos) == Point_getXOffset(pos2) );
    fail_unless(Point_getYOffset(pos) == Point_getYOffset(pos2) );
    fail_unless(Point_getZOffset(pos) == Point_getZOffset(pos2) );
}
END_TEST

START_TEST ( test_BoundingBox_setPosition_NULL )
{
    BoundingBox_setPosition(BB,NULL);
    Point_t *pos=BoundingBox_getPosition(BB);
    fail_unless(pos != NULL);
    fail_unless(Point_getXOffset(pos) == 0.0 );
    fail_unless(Point_getYOffset(pos) == 0.0 );
    fail_unless(Point_getZOffset(pos) == 0.0 );
}
END_TEST

START_TEST ( test_BoundingBox_setDimensions )
{
    Dimensions_t *dim=Dimensions_createWithSize(-4.4,5.5,-6.6);
    BoundingBox_setDimensions(BB,dim);
    Dimensions_t *dim2=BoundingBox_getDimensions(BB);
    fail_unless(dim2 != NULL);
    fail_unless(Dimensions_getWidth (dim) == Dimensions_getWidth (dim2) );
    fail_unless(Dimensions_getHeight(dim) == Dimensions_getHeight(dim2) );
    fail_unless(Dimensions_getDepth (dim) == Dimensions_getDepth (dim2) );
}
END_TEST

START_TEST ( test_BoundingBox_setDimensions_NULL )
{
    BoundingBox_setDimensions(BB,NULL);
    Dimensions_t *dim=BoundingBox_getDimensions(BB);
    fail_unless(dim != NULL);
    fail_unless(Dimensions_getWidth (dim) == 0.0 );
    fail_unless(Dimensions_getHeight(dim) == 0.0 );
    fail_unless(Dimensions_getDepth (dim) == 0.0 );
}
END_TEST

Suite *
create_suite_BoundingBox (void)
{
  Suite *suite = suite_create("BoundingBox");
  TCase *tcase = tcase_create("BoundingBox");


  tcase_add_checked_fixture( tcase,
                             BoundingBoxTest_setup,
                             BoundingBoxTest_teardown );

  tcase_add_test( tcase, test_BoundingBox_create                     );
  tcase_add_test( tcase, test_BoundingBox_createWith                 );
  tcase_add_test( tcase, test_BoundingBox_createWith_NULL            );
  tcase_add_test( tcase, test_BoundingBox_createWithCoordinates_NULL );
  tcase_add_test( tcase, test_BoundingBox_free_NULL                  );
  tcase_add_test( tcase, test_BoundingBox_setId                      );
  tcase_add_test( tcase, test_BoundingBox_setId_NULL                 );
  tcase_add_test( tcase, test_BoundingBox_setPosition                );
  tcase_add_test( tcase, test_BoundingBox_setPosition_NULL           );
  tcase_add_test( tcase, test_BoundingBox_setDimensions              );
  tcase_add_test( tcase, test_BoundingBox_setDimensions_NULL         );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
