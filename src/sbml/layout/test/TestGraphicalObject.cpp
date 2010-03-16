/**
 * Filename    : TestGraphicalObject.cpp
 * Description : Unit tests for GraphicalObject
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

#include "GraphicalObject.h"
#include "BoundingBox.h"
#include "Dimensions.h"
#include "Point.h"

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static GraphicalObject* GO;

void
GraphicalObjectTest_setup (void)
{
    GO = new (std::nothrow) GraphicalObject();

    if (GO == NULL)
    {
        fail("new (std::nothrow) GraphicalObject returned a NULL pointer.");
    }

}

void 
GraphicalObjectTest_teardown (void)
{
    delete GO;
}

START_TEST (test_GraphicalObject_new)
{
    fail_unless( GO->getTypeCode()    == SBML_LAYOUT_GRAPHICALOBJECT );
    fail_unless( GO->getMetaId()      == "" );
//    fail_unless( GO->getNotes()       == "" );
//    fail_unless( GO->getAnnotation()  == "" );
    fail_unless( GO->getId()          == "" );
    fail_unless( !GO->isSetId());
    const BoundingBox& bb=*(GO->getBoundingBox());
    const Point& pos=*(bb.getPosition());
    const Dimensions& dim=*(bb.getDimensions());
    fail_unless(pos.getXOffset() == 0.0);
    fail_unless(pos.getYOffset() == 0.0);
    fail_unless(pos.getZOffset() == 0.0);
    fail_unless(dim.getWidth()  == 0.0);
    fail_unless(dim.getHeight() == 0.0);
    fail_unless(dim.getDepth()  == 0.0);

}
END_TEST

START_TEST ( test_GraphicalObject_new_WithLevelVersionAndNamespaces )
{
   unsigned int level=1;
   unsigned int version=2;
   GraphicalObject *go=new GraphicalObject(level, version); 
   fail_unless( go->getTypeCode() == SBML_LAYOUT_GRAPHICALOBJECT );
   fail_unless( go->getMetaId()   == "" );

   fail_unless(go->getLevel() == level);
   fail_unless(go->getVersion() == version);

   fail_unless( go->isSetId() == false );
   const BoundingBox& bb=*(go->getBoundingBox());
   const Point& pos=*(bb.getPosition());
   const Dimensions& dim=*(bb.getDimensions());
   fail_unless(pos.getXOffset() == 0.0);
   fail_unless(pos.getYOffset() == 0.0);
   fail_unless(pos.getZOffset() == 0.0);
   fail_unless(dim.getWidth()  == 0.0);
   fail_unless(dim.getHeight() == 0.0);
   fail_unless(dim.getDepth()  == 0.0);
   delete go;

   level=2;
   version=3;
   go=new GraphicalObject(level, version); 
   fail_unless( go->getTypeCode() == SBML_LAYOUT_GRAPHICALOBJECT );
   fail_unless( go->getMetaId()   == "" );

   fail_unless(go->getLevel() == level);
   fail_unless(go->getVersion() == version);

   fail_unless( go->isSetId() == false );
   const BoundingBox& bb2=*(go->getBoundingBox());
   const Point& pos2=*(bb2.getPosition());
   const Dimensions& dim2=*(bb2.getDimensions());
   fail_unless(pos2.getXOffset() == 0.0);
   fail_unless(pos2.getYOffset() == 0.0);
   fail_unless(pos2.getZOffset() == 0.0);
   fail_unless(dim2.getWidth()  == 0.0);
   fail_unless(dim2.getHeight() == 0.0);
   fail_unless(dim2.getDepth()  == 0.0);
   delete go;
}
END_TEST

START_TEST ( test_GraphicalObject_new_WithNamespace )
{
   SBMLNamespaces* ns = new SBMLNamespaces;
   GraphicalObject *go=new GraphicalObject(ns); 
   fail_unless( go->getTypeCode() == SBML_LAYOUT_GRAPHICALOBJECT );
   fail_unless( go->getMetaId()   == "" );

   fail_unless(go->getLevel() == SBML_DEFAULT_LEVEL);
   fail_unless(go->getVersion() == SBML_DEFAULT_VERSION);

   fail_unless( go->isSetId() == false );
   const BoundingBox& bb=*(go->getBoundingBox());
   const Point& pos=*(bb.getPosition());
   const Dimensions& dim=*(bb.getDimensions());
   fail_unless(pos.getXOffset() == 0.0);
   fail_unless(pos.getYOffset() == 0.0);
   fail_unless(pos.getZOffset() == 0.0);
   fail_unless(dim.getWidth()  == 0.0);
   fail_unless(dim.getHeight() == 0.0);
   fail_unless(dim.getDepth()  == 0.0);
   delete go;
   delete ns;

   ns = new SBMLNamespaces(2,3);
   go=new GraphicalObject(ns); 
   fail_unless( go->getTypeCode() == SBML_LAYOUT_GRAPHICALOBJECT );
   fail_unless( go->getMetaId()   == "" );

   fail_unless(go->getLevel() == 2);
   fail_unless(go->getVersion() == 3);

   fail_unless( go->isSetId() == false );
   const BoundingBox& bb2=*(go->getBoundingBox());
   const Point& pos2=*(bb2.getPosition());
   const Dimensions& dim2=*(bb2.getDimensions());
   fail_unless(pos2.getXOffset() == 0.0);
   fail_unless(pos2.getYOffset() == 0.0);
   fail_unless(pos2.getZOffset() == 0.0);
   fail_unless(dim2.getWidth()  == 0.0);
   fail_unless(dim2.getHeight() == 0.0);
   fail_unless(dim2.getDepth()  == 0.0);
   delete go;
   delete ns;
}
END_TEST


START_TEST (test_GraphicalObject_new_with_id)
{
    std::string id="TestGraphicalObject";
    GraphicalObject* go=new GraphicalObject(id);
    fail_unless( go->getTypeCode()    == SBML_LAYOUT_GRAPHICALOBJECT );
    fail_unless( go->getMetaId()      == "" );
//    fail_unless( go->getNotes()       == "" );
//    fail_unless( go->getAnnotation() == "" );
    fail_unless( go->isSetId());
    fail_unless( go->getId() == id );
    
    const BoundingBox& bb=*(go->getBoundingBox());
    const Point& pos=*(bb.getPosition());
    const Dimensions& dim=*(bb.getDimensions());
    fail_unless(pos.getXOffset() == 0.0);
    fail_unless(pos.getYOffset() == 0.0);
    fail_unless(pos.getZOffset() == 0.0);
    fail_unless(dim.getWidth()  == 0.0);
    fail_unless(dim.getHeight() == 0.0);
    fail_unless(dim.getDepth()  == 0.0);

    delete go;
}
END_TEST

START_TEST (test_GraphicalObject_new_with_id_and_2D_coordinates)
{
    std::string id="TestGraphicalObject";
    GraphicalObject* go=new GraphicalObject(id,1.1,-2.2,3.3,-4.4);
    fail_unless( go->getTypeCode()    == SBML_LAYOUT_GRAPHICALOBJECT );
    fail_unless( go->getMetaId()      == "" );
//    fail_unless( go->getNotes()       == "" );
//    fail_unless( go->getAnnotation() == "" );
    fail_unless( go->isSetId());
    fail_unless( go->getId() == id );
    
    const BoundingBox& bb=*(go->getBoundingBox());
    const Point& pos=*(bb.getPosition());
    const Dimensions& dim=*(bb.getDimensions());
    fail_unless(pos.getXOffset() ==  1.1);
    fail_unless(pos.getYOffset() == -2.2);
    fail_unless(pos.getZOffset() ==  0.0);
    fail_unless(dim.getWidth()  ==  3.3);
    fail_unless(dim.getHeight() == -4.4);
    fail_unless(dim.getDepth()  ==  0.0);

    delete go;
}
END_TEST


START_TEST (test_GraphicalObject_new_with_id_and_3D_coordinates)
{
    std::string id="TestGraphicalObject";
    GraphicalObject* go=new GraphicalObject(id,1.1,-2.2,3.3,-4.4,5.5,-6.6);
    fail_unless( go->getTypeCode()    == SBML_LAYOUT_GRAPHICALOBJECT );
    fail_unless( go->getMetaId()      == "" );
//    fail_unless( go->getNotes()       == "" );
//    fail_unless( go->getAnnotation() == "" );
    fail_unless( go->isSetId());
    fail_unless( go->getId() == id );
    
    const BoundingBox& bb=*(go->getBoundingBox());
    const Point& pos=*(bb.getPosition());
    const Dimensions& dim=*(bb.getDimensions());
    fail_unless(pos.getXOffset() ==  1.1);
    fail_unless(pos.getYOffset() == -2.2);
    fail_unless(pos.getZOffset() ==  3.3);
    fail_unless(dim.getWidth()  == -4.4);
    fail_unless(dim.getHeight() ==  5.5);
    fail_unless(dim.getDepth()  == -6.6);

    delete go;

}
END_TEST

START_TEST (test_GraphicalObject_new_with_id_point_and_dimensions)
{
    Point pos2=Point(1.1,-2.2,3.3);
    Dimensions dim2=Dimensions(-4.4,5.5,-6.6);
    std::string id="TestGraphicalObject";
    GraphicalObject* go=new GraphicalObject(id,&pos2,&dim2);
    fail_unless( go->getTypeCode()    == SBML_LAYOUT_GRAPHICALOBJECT );
    fail_unless( go->getMetaId()      == "" );
//    fail_unless( go->getNotes()       == "" );
//    fail_unless( go->getAnnotation() == "" );
    fail_unless( go->isSetId());
    fail_unless( go->getId() == id );
    
    const BoundingBox& bb=*(go->getBoundingBox());
    const Point& pos=*(bb.getPosition());
    const Dimensions& dim=*(bb.getDimensions());
    fail_unless(pos.getXOffset() == pos2.getXOffset());
    fail_unless(pos.getYOffset() == pos2.getYOffset());
    fail_unless(pos.getZOffset() == pos2.getZOffset());
    fail_unless(dim.getWidth  () == dim2.getWidth  ());
    fail_unless(dim.getHeight () == dim2.getHeight ());
    fail_unless(dim.getDepth  () == dim2.getDepth  ());

    delete go;
}
END_TEST

START_TEST (test_GraphicalObject_new_with_id_and_boundingbox )
{
    BoundingBox bb2=BoundingBox();
    Point pos2=Point(1.1,-2.2,3.3);
    bb2.setPosition(&pos2);
    Dimensions dim2=Dimensions(-4.4,5.5,-6.6);
    bb2.setDimensions(&dim2);
    std::string id="TestGraphicalObject";
    GraphicalObject* go=new GraphicalObject(id,&bb2);
    fail_unless( go->getTypeCode()    == SBML_LAYOUT_GRAPHICALOBJECT );
    fail_unless( go->getMetaId()      == "" );
//    fail_unless( go->getNotes()       == "" );
//    fail_unless( go->getAnnotation() == "" );
    fail_unless( go->isSetId());
    fail_unless( go->getId() == id );
    
    const BoundingBox& bb=*(go->getBoundingBox());
    const Point& pos=*(bb.getPosition());
    const Dimensions& dim=*(bb.getDimensions());
    fail_unless(pos.getXOffset() == pos2.getXOffset());
    fail_unless(pos.getYOffset() == pos2.getYOffset());
    fail_unless(pos.getZOffset() == pos2.getZOffset());
    fail_unless(dim.getWidth()  == dim2.getWidth());
    fail_unless(dim.getHeight() == dim2.getHeight());
    fail_unless(dim.getDepth()  == dim2.getDepth());

    delete go;

}
END_TEST

START_TEST (test_GraphicalObject_setId )
{
    std::string id="TestGraphicalObject";
    GO->setId(id);
    fail_unless(GO->isSetId());
    fail_unless(GO->getId() == id); 
}
END_TEST

START_TEST (test_GraphicalObject_setBoundingBox)
{
    BoundingBox bb=BoundingBox();
    Point* p=new Point(1.1,-2.2,3.3);
    bb.setPosition(p);
    delete p;
    Dimensions d=Dimensions(-4.4,5.5,-6.6);
    bb.setDimensions(&d);
    GO->setBoundingBox(&bb);
    const BoundingBox& bb2=*(GO->getBoundingBox());
    fail_unless(bb.getPosition()->getXOffset() == bb2.getPosition()->getXOffset());
    fail_unless(bb.getPosition()->getYOffset() == bb2.getPosition()->getYOffset());
    fail_unless(bb.getPosition()->getZOffset() == bb2.getPosition()->getZOffset());
    fail_unless(bb.getDimensions()->getWidth() == bb2.getDimensions()->getWidth());
    fail_unless(bb.getDimensions()->getHeight() == bb2.getDimensions()->getHeight());
    fail_unless(bb.getDimensions()->getDepth() == bb2.getDimensions()->getDepth());
}
END_TEST

START_TEST ( test_GraphicalObject_copyConstructor )
{
    GraphicalObject* go1=new GraphicalObject();
    XMLNode* notes=new XMLNode();
    go1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    go1->setAnnotation(annotation);
    GraphicalObject* go2=new GraphicalObject(*go1);
    delete go2;
    delete go1;
}
END_TEST

START_TEST ( test_GraphicalObject_assignmentOperator )
{
    GraphicalObject* go1=new GraphicalObject();
    XMLNode* notes=new XMLNode();
    go1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    go1->setAnnotation(annotation);
    GraphicalObject* go2=new GraphicalObject();
    (*go2)=(*go1);
    delete go2;
    delete go1;
}
END_TEST


Suite *
create_suite_GraphicalObject (void)
{
  Suite *suite = suite_create("GraphicalObject");
  TCase *tcase = tcase_create("GraphicalObject");

  tcase_add_checked_fixture( tcase,
                             GraphicalObjectTest_setup,
                             GraphicalObjectTest_teardown );

  tcase_add_test( tcase, test_GraphicalObject_new                               );
  tcase_add_test( tcase, test_GraphicalObject_new_WithLevelVersionAndNamespaces );
  tcase_add_test( tcase, test_GraphicalObject_new_WithNamespace                 );
  tcase_add_test( tcase, test_GraphicalObject_new_with_id                       );
  tcase_add_test( tcase, test_GraphicalObject_new_with_id_and_2D_coordinates    );
  tcase_add_test( tcase, test_GraphicalObject_new_with_id_and_3D_coordinates    );
  tcase_add_test( tcase, test_GraphicalObject_new_with_id_point_and_dimensions  );
  tcase_add_test( tcase, test_GraphicalObject_new_with_id_and_boundingbox       );
  tcase_add_test( tcase, test_GraphicalObject_setId                             );
  tcase_add_test( tcase, test_GraphicalObject_setBoundingBox                    );
  tcase_add_test( tcase, test_GraphicalObject_copyConstructor                   );
  tcase_add_test( tcase, test_GraphicalObject_assignmentOperator                );

  suite_add_tcase(suite, tcase);

  return suite;
}



END_C_DECLS
