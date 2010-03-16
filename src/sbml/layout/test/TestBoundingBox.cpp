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


#include "common/common.h"


#include "BoundingBox.h"

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

static BoundingBox* BB;

static void
BoundingBoxTest_setup (void)
{
    BB = new BoundingBox();

    if(BB == NULL)
    {
        fail("BoundingBox(); returned a NULL pointer.");
    }
}

static void
BoundingBoxTest_teardown (void)
{
    delete BB;
}


CK_CPPSTART

START_TEST ( test_BoundingBox_create )
{
   fail_unless( BB->getTypeCode() == SBML_LAYOUT_BOUNDINGBOX );
   fail_unless( BB->getMetaId() == "" );
//   fail_unless( SBase_getNotes      ((SBase_t*) BB) == NULL );
//   fail_unless( SBase_getAnnotation ((SBase_t*) BB) == NULL );

   fail_unless(BB->isSetId() == false );
   
   Point* pos=BB->getPosition();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   Dimensions *dim=BB->getDimensions(); 
   fail_unless(dim != NULL);
   fail_unless(dim->getWidth () == 0.0);  
   fail_unless(dim->getHeight() == 0.0);  
   fail_unless(dim->getDepth () == 0.0);  
 
}
END_TEST

START_TEST ( test_BoundingBox_new_WithLevelVersionAndNamespaces )
{
   unsigned int level=1;
   unsigned int version=2;
   BoundingBox *bb=new BoundingBox(level, version); 
   fail_unless( bb->getTypeCode() == SBML_LAYOUT_BOUNDINGBOX );
   fail_unless( bb->getMetaId()   == "" );

   fail_unless(bb->getLevel() == level);
   fail_unless(bb->getVersion() == version);

   fail_unless( bb->isSetId() == false );
   
   Point *pos=bb->getPosition();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   Dimensions *dim=bb->getDimensions(); 
   fail_unless(dim != NULL);
   fail_unless(dim->getWidth () == 0.0);  
   fail_unless(dim->getHeight() == 0.0);  
   fail_unless(dim->getDepth () == 0.0);  
   delete bb;

   level=2;
   version=3;
   bb=new BoundingBox(level, version); 
   fail_unless( bb->getTypeCode() == SBML_LAYOUT_BOUNDINGBOX );
   fail_unless( bb->getMetaId()   == "" );

   fail_unless(bb->getLevel() == level);
   fail_unless(bb->getVersion() == version);

   fail_unless( bb->isSetId() == false );
   
   pos=bb->getPosition();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   dim=bb->getDimensions(); 
   fail_unless(dim != NULL);
   fail_unless(dim->getWidth () == 0.0);  
   fail_unless(dim->getHeight() == 0.0);  
   fail_unless(dim->getDepth () == 0.0);  
   delete bb;
}
END_TEST

START_TEST ( test_BoundingBox_new_WithNamespace )
{
   SBMLNamespaces* ns=new SBMLNamespaces;
   BoundingBox *bb=new BoundingBox(ns); 
   fail_unless( bb->getTypeCode() == SBML_LAYOUT_BOUNDINGBOX );
   fail_unless( bb->getMetaId()   == "" );

   fail_unless(bb->getLevel() == SBML_DEFAULT_LEVEL);
   fail_unless(bb->getVersion() == SBML_DEFAULT_VERSION);

   fail_unless( bb->isSetId() == false );
   
   Point *pos=bb->getPosition();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   Dimensions *dim=bb->getDimensions(); 
   fail_unless(dim != NULL);
   fail_unless(dim->getWidth () == 0.0);  
   fail_unless(dim->getHeight() == 0.0);  
   fail_unless(dim->getDepth () == 0.0);  
   delete bb;
   delete ns;

   ns = new SBMLNamespaces(2,3);
   bb=new BoundingBox(ns); 
   fail_unless( bb->getTypeCode() == SBML_LAYOUT_BOUNDINGBOX );
   fail_unless( bb->getMetaId()   == "" );

   fail_unless(bb->getLevel() == 2);
   fail_unless(bb->getVersion() == 3);

   fail_unless( bb->isSetId() == false );
   
   pos=bb->getPosition();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   dim=bb->getDimensions(); 
   fail_unless(dim != NULL);
   fail_unless(dim->getWidth () == 0.0);  
   fail_unless(dim->getHeight() == 0.0);  
   fail_unless(dim->getDepth () == 0.0);  
   delete bb;
   delete ns;
}
END_TEST



START_TEST ( test_BoundingBox_createWith )
{
   const char* id="BoundingBox"; 
   BoundingBox *bb=new BoundingBox(id); 
   fail_unless( bb->getTypeCode() == SBML_LAYOUT_BOUNDINGBOX );
   fail_unless( bb->getMetaId()   == "" );
//   fail_unless( SBase_getNotes      ((SBase_t*) bb) == NULL );
//   fail_unless( SBase_getAnnotation ((SBase_t*) bb) == NULL );

   fail_unless( bb->isSetId() == true );
   fail_unless( bb->getId() == id);
   
   Point *pos=bb->getPosition();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   Dimensions *dim=bb->getDimensions(); 
   fail_unless(dim != NULL);
   fail_unless(dim->getWidth () == 0.0);  
   fail_unless(dim->getHeight() == 0.0);  
   fail_unless(dim->getDepth () == 0.0);  
   delete bb;
}
END_TEST

START_TEST ( test_BoundingBox_createWith_NULL )
{
   BoundingBox *bb=new BoundingBox(""); 
   fail_unless( bb->getTypeCode()    == SBML_LAYOUT_BOUNDINGBOX );
   fail_unless( bb->getMetaId()  == "" );
//   fail_unless( SBase_getNotes      ((SBase_t*) bb) == NULL );
//   fail_unless( SBase_getAnnotation ((SBase_t*) bb) == NULL );

   fail_unless( bb->isSetId() == false );
   
   Point *pos=bb->getPosition();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() == 0.0);  
   fail_unless(pos->getYOffset() == 0.0);  
   fail_unless(pos->getZOffset() == 0.0);  
   
   Dimensions *dim=bb->getDimensions(); 
   fail_unless(dim != NULL);
   fail_unless(dim->getWidth () == 0.0);  
   fail_unless(dim->getHeight() == 0.0);  
   fail_unless(dim->getDepth () == 0.0);  

   delete bb;
 }
END_TEST

START_TEST ( test_BoundingBox_createWithCoordinates )
{
   const char* id="BoundingBox";
   BoundingBox *bb=new BoundingBox(id,1.1,-2.2,3.3,-4.4,5.5,-6.6);
   fail_unless( bb->getTypeCode() == SBML_LAYOUT_BOUNDINGBOX );
   fail_unless( bb->getMetaId() == "" );
//   fail_unless( SBase_getNotes      ((SBase_t*) bb) == NULL );
//   fail_unless( SBase_getAnnotation ((SBase_t*) bb) == NULL );

   fail_unless( bb->isSetId() == true );
   fail_unless( bb->getId() == id);
   
   Point *pos=bb->getPosition();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() ==  1.1);  
   fail_unless(pos->getYOffset() == -2.2);  
   fail_unless(pos->getZOffset() ==  3.3);  
   
   Dimensions *dim=bb->getDimensions(); 
   fail_unless(dim != NULL);
   fail_unless(dim->getWidth () == -4.4);  
   fail_unless(dim->getHeight() ==  5.5);  
   fail_unless(dim->getDepth () == -6.6);  

   delete bb;
 }
END_TEST

START_TEST ( test_BoundingBox_createWithCoordinates_NULL )
{
   BoundingBox *bb=new BoundingBox("",1.1,-2.2,3.3,-4.4,5.5,-6.6);
   fail_unless( bb->getTypeCode() == SBML_LAYOUT_BOUNDINGBOX );
   fail_unless( bb->getMetaId() == "" );
//   fail_unless( SBase_getNotes      ((SBase_t*) bb) == NULL );
//   fail_unless( SBase_getAnnotation ((SBase_t*) bb) == NULL );

   fail_unless( bb->isSetId() == false );
   
   Point *pos=bb->getPosition();
   fail_unless(pos != NULL);
   fail_unless(pos->getXOffset() ==  1.1);  
   fail_unless(pos->getYOffset() == -2.2);  
   fail_unless(pos->getZOffset() ==  3.3);  
   
   Dimensions *dim=bb->getDimensions(); 
   fail_unless(dim != NULL);
   fail_unless(dim->getWidth () == -4.4);  
   fail_unless(dim->getHeight() ==  5.5);  
   fail_unless(dim->getDepth () == -6.6);  

   delete bb;
}
END_TEST

START_TEST ( test_BoundingBox_setId )
{
    const char* id="BoundingBox";
    BB->setId(id);
    fail_unless(BB->isSetId() == true);
    fail_unless(BB->getId() == id);
}
END_TEST

START_TEST ( test_BoundingBox_setId_NULL )
{
    BB->setId("");
    fail_unless(BB->isSetId() == false);
    fail_unless(BB->getId() == "");
}
END_TEST

START_TEST ( test_BoundingBox_setPosition )
{
    Point pos=Point(-1.1,2.2,-3.3);
    BB->setPosition(&pos);
    Point *pos2=BB->getPosition();
    fail_unless(pos2 != NULL);
    fail_unless(pos.getXOffset() == pos2->getXOffset() );
    fail_unless(pos.getYOffset() == pos2->getYOffset() );
    fail_unless(pos.getZOffset() == pos2->getZOffset() );
}
END_TEST

START_TEST ( test_BoundingBox_setPosition_NULL )
{
    BB->setPosition(NULL);
    Point *pos=BB->getPosition();
    fail_unless(pos != NULL);
    fail_unless(pos->getXOffset() == 0.0 );
    fail_unless(pos->getYOffset() == 0.0 );
    fail_unless(pos->getZOffset() == 0.0 );
}
END_TEST

START_TEST ( test_BoundingBox_setDimensions )
{
    Dimensions dim=Dimensions(-4.4,5.5,-6.6);
    BB->setDimensions(&dim);
    Dimensions *dim2=BB->getDimensions();
    fail_unless(dim2 != NULL);
    fail_unless(dim.getWidth () == dim2->getWidth () );
    fail_unless(dim.getHeight() == dim2->getHeight() );
    fail_unless(dim.getDepth () == dim2->getDepth () );
}
END_TEST

START_TEST ( test_BoundingBox_setDimensions_NULL )
{
    BB->setDimensions(NULL);
    Dimensions *dim=BB->getDimensions();
    fail_unless(dim != NULL);
    fail_unless(dim->getWidth () == 0.0 );
    fail_unless(dim->getHeight() == 0.0 );
    fail_unless(dim->getDepth () == 0.0 );
}
END_TEST

START_TEST ( test_BoundingBox_copyConstructor )
{
    BoundingBox* bb1=new BoundingBox();
    XMLNode* notes=new XMLNode();
    bb1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    bb1->setAnnotation(annotation);
    BoundingBox* bb2=new BoundingBox(*bb1);
    delete bb2;
    delete bb1;
}
END_TEST

START_TEST ( test_BoundingBox_assignmentOperator )
{
    BoundingBox* bb1=new BoundingBox();
    XMLNode* notes=new XMLNode();
    bb1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    bb1->setAnnotation(annotation);
    BoundingBox* bb2=new BoundingBox();
    (*bb2)=(*bb1);
    delete bb2;
    delete bb1;
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

  tcase_add_test( tcase, test_BoundingBox_create                            );
  tcase_add_test( tcase, test_BoundingBox_new_WithLevelVersionAndNamespaces );
  tcase_add_test( tcase, test_BoundingBox_new_WithNamespace                 );
  tcase_add_test( tcase, test_BoundingBox_createWith                        );
  tcase_add_test( tcase, test_BoundingBox_createWith_NULL                   );
  tcase_add_test( tcase, test_BoundingBox_createWithCoordinates             );
  tcase_add_test( tcase, test_BoundingBox_createWithCoordinates_NULL        );
  tcase_add_test( tcase, test_BoundingBox_setId                             );
  tcase_add_test( tcase, test_BoundingBox_setId_NULL                        );
  tcase_add_test( tcase, test_BoundingBox_setPosition                       );
  tcase_add_test( tcase, test_BoundingBox_setPosition_NULL                  );
  tcase_add_test( tcase, test_BoundingBox_setDimensions                     );
  tcase_add_test( tcase, test_BoundingBox_setDimensions_NULL                );
  tcase_add_test( tcase, test_BoundingBox_copyConstructor                   );
  tcase_add_test( tcase, test_BoundingBox_assignmentOperator                );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
