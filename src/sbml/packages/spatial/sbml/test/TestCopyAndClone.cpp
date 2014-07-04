/**
 * \file    TestCopyAndClone.cpp
 * \brief   Copy SBML unit tests
 * \author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 * 
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/common/common.h>

#include <sbml/SBase.h>
#include <sbml/packages/spatial/sbml/PolygonObject.h>
#include <sbml/packages/spatial/sbml/ParametricObject.h>


#include <check.h>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond */


CK_CPPSTART

static SpatialPkgNamespaces* GNS = new SpatialPkgNamespaces();

START_TEST ( test_PolygonObject_copyConstructor )
{
  PolygonObject* o1=new PolygonObject(GNS);
  int points [] = {1,2,3};
  o1->setPointIndices(points, 3);
  
  fail_unless(o1->getIndicesLength() == 3);

  PolygonObject* o2=new PolygonObject(*o1);

  fail_unless(o2->getIndicesLength() == 3);

  //fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  //delete o2;
  //delete o1;
}
END_TEST


START_TEST ( test_PolygonObject_assignmentOperator )
{
  PolygonObject* o1=new PolygonObject(GNS);
  int points [] = {1,2,3};
  o1->setPointIndices(points, 3);
  
  fail_unless(o1->getIndicesLength() == 3);
  
  PolygonObject* o2 = new PolygonObject(GNS);;
  (*o2)=*o1;

  fail_unless(o2->getIndicesLength() == 3);

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  //delete o2;
  //delete o1;
}
END_TEST


START_TEST ( test_PolygonObject_clone )
{
  PolygonObject* o1=new PolygonObject(GNS);
  int points [] = {1,2,3};
  o1->setPointIndices(points, 3);
  
  fail_unless(o1->getIndicesLength() == 3);

  PolygonObject* o2=o1->clone();
 
  fail_unless(o2->getIndicesLength() == 3);

  //fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  //delete o2;
  //delete o1;
}
END_TEST


START_TEST ( test_ParametricObject_copyConstructor )
{
  ParametricObject* o1=new ParametricObject(GNS);
  o1->setId("s");
  
  fail_unless(o1->getId() == "s");

  //PolygonObject* obj=new PolygonObject(GNS);
  //o1->setPolygonObject(obj);

  //fail_unless(o1->isSetPolygonObject() == true);

  ParametricObject* o2=new ParametricObject(*o1);

  fail_unless(o2->getId() == "s");
  //fail_unless(o2->isSetPolygonObject() == true);

  //fail_unless(o2->getPolygonObject() != o1->getPolygonObject());

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_ParametricObject_assignmentOperator )
{
  ParametricObject* o1=new ParametricObject(GNS);
  o1->setId("s");
  
  fail_unless(o1->getId() == "s");
 
  //PolygonObject* obj=new PolygonObject(GNS);
  //o1->setPolygonObject(obj);

  //fail_unless(o1->isSetPolygonObject() == true);

  ParametricObject* o2 = new ParametricObject(GNS);;
  (*o2)=*o1;

  fail_unless(o2->getId() == "s");
  //fail_unless(o2->isSetPolygonObject() == true);

  //fail_unless(o2->getPolygonObject() != o1->getPolygonObject());

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_ParametricObject_clone )
{
  ParametricObject* o1=new ParametricObject(GNS);
  o1->setId("s");
  
  fail_unless(o1->getId() == "s");

  //PolygonObject* obj=new PolygonObject(GNS);
  //o1->setPolygonObject(obj);

  //fail_unless(o1->isSetPolygonObject() == true);

  ParametricObject* o2=o1->clone();
 
  fail_unless(o2->getId() == "s");
  //fail_unless(o2->isSetPolygonObject() == true);

  //fail_unless(o2->getPolygonObject() != o1->getPolygonObject());

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


Suite *
create_suite_CopyAndClone (void)
{
  Suite *suite = suite_create("CopyAndClone");
  TCase *tcase = tcase_create("CopyAndClone");

  tcase_add_test( tcase, test_PolygonObject_copyConstructor );
  tcase_add_test( tcase, test_PolygonObject_assignmentOperator );
  tcase_add_test( tcase, test_PolygonObject_clone );
  tcase_add_test( tcase, test_ParametricObject_copyConstructor );
  tcase_add_test( tcase, test_ParametricObject_assignmentOperator );
  tcase_add_test( tcase, test_ParametricObject_clone );

  suite_add_tcase(suite, tcase);

  return suite;
}
CK_CPPEND

