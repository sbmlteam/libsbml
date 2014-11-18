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
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>


#include <check.h>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond */


CK_CPPSTART

static SpatialPkgNamespaces* GNS = NULL;


void
SpatialCopyAndClone_setup (void)
{
  GNS = new SpatialPkgNamespaces();
}

void 
SpatialCopyAndClone_teardown (void)
{
  delete GNS;
}


START_TEST ( test_PolygonObject_copyConstructor )
{
  PolygonObject* o1=new PolygonObject(GNS);
  int points [] = {1,2,3};
  o1->setPointIndex(points, 3);
  
  fail_unless(o1->getPointIndexLength() == 3);

  PolygonObject* o2=new PolygonObject(*o1);

  fail_unless(o2->getPointIndexLength() == 3);

  int pointsRet [] = {0, 0, 0};
  o2->getPointIndex(pointsRet);
  
  fail_unless(pointsRet[0] == 1);
  fail_unless(pointsRet[1] == 2);
  fail_unless(pointsRet[2] == 3);

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_PolygonObject_assignmentOperator )
{
  PolygonObject* o1=new PolygonObject(GNS);
  int points [] = {1,2,3};
  o1->setPointIndex(points, 3);
  
  fail_unless(o1->getPointIndexLength() == 3);
  
  PolygonObject* o2 = new PolygonObject(GNS);;
  (*o2)=*o1;

  fail_unless(o2->getPointIndexLength() == 3);

  int pointsRet [] = {0, 0, 0};
  o2->getPointIndex(pointsRet);
  
  fail_unless(pointsRet[0] == 1);
  fail_unless(pointsRet[1] == 2);
  fail_unless(pointsRet[2] == 3);

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_PolygonObject_clone )
{
  PolygonObject* o1=new PolygonObject(GNS);
  int points [] = {1,2,3};
  o1->setPointIndex(points, 3);
  
  fail_unless(o1->getPointIndexLength() == 3);

  PolygonObject* o2=o1->clone();
 
  fail_unless(o2->getPointIndexLength() == 3);

  int pointsRet [] = {0, 0, 0};
  o2->getPointIndex(pointsRet);
  
  fail_unless(pointsRet[0] == 1);
  fail_unless(pointsRet[1] == 2);
  fail_unless(pointsRet[2] == 3);

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_ParametricObject_copyConstructor )
{
  ParametricObject* o1=new ParametricObject(GNS);
  o1->setId("s");
  
  fail_unless(o1->getId() == "s");

  PolygonObject* obj=new PolygonObject(GNS);
  o1->setPolygonObject(obj);

  fail_unless(o1->isSetPolygonObject() == true);

  ParametricObject* o2=new ParametricObject(*o1);

  fail_unless(o2->getId() == "s");
  fail_unless(o2->isSetPolygonObject() == true);

  fail_unless(o2->getPolygonObject() != o1->getPolygonObject());

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o1;
  delete o2;
  delete obj;
}
END_TEST


START_TEST ( test_ParametricObject_assignmentOperator )
{
  ParametricObject* o1=new ParametricObject(GNS);
  o1->setId("s");
  
  fail_unless(o1->getId() == "s");
 
  PolygonObject* obj=new PolygonObject(GNS);
  o1->setPolygonObject(obj);

  fail_unless(o1->isSetPolygonObject() == true);

  ParametricObject* o2 = new ParametricObject(GNS);;
  (*o2)=*o1;

  fail_unless(o2->getId() == "s");
  fail_unless(o2->isSetPolygonObject() == true);

  fail_unless(o2->getPolygonObject() != o1->getPolygonObject());

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
  delete obj;
}
END_TEST


START_TEST ( test_ParametricObject_clone )
{
  ParametricObject* o1=new ParametricObject(GNS);
  o1->setId("s");
  
  fail_unless(o1->getId() == "s");

  PolygonObject* obj=new PolygonObject(GNS);
  o1->setPolygonObject(obj);

  fail_unless(o1->isSetPolygonObject() == true);

  ParametricObject* o2=o1->clone();
 
  fail_unless(o2->getId() == "s");
  fail_unless(o2->isSetPolygonObject() == true);

  fail_unless(o2->getPolygonObject() != o1->getPolygonObject());

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
  delete obj;
}
END_TEST


START_TEST ( test_SampledField_copyConstructor )
{
  SampledField* o1=new SampledField(GNS);
  int points [] = {1,2,3};
  o1->setSamples(points, 3);
  
  fail_unless(o1->getSamplesLength() == 3);

  SampledField* o2=new SampledField(*o1);

  fail_unless(o2->getSamplesLength() == 3);

  int samplesRet [] = {0, 0, 0};
  o2->getSamples(samplesRet);
  
  fail_unless(samplesRet[0] == 1);
  fail_unless(samplesRet[1] == 2);
  fail_unless(samplesRet[2] == 3);

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_SampledField_assignmentOperator )
{
  SampledField* o1=new SampledField(GNS);
  int points [] = {1,2,3};
  o1->setSamples(points, 3);
  
  fail_unless(o1->getSamplesLength() == 3);
  
  SampledField* o2 = new SampledField(GNS);;
  (*o2)=*o1;

  fail_unless(o2->getSamplesLength() == 3);

  int samplesRet [] = {0, 0, 0};
  o2->getSamples(samplesRet);
  
  fail_unless(samplesRet[0] == 1);
  fail_unless(samplesRet[1] == 2);
  fail_unless(samplesRet[2] == 3);

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_SampledField_clone )
{
  SampledField* o1=new SampledField(GNS);
  int points [] = {1,2,3};
  o1->setSamples(points, 3);
  
  fail_unless(o1->getSamplesLength() == 3);

  SampledField* o2=o1->clone();
 
  fail_unless(o2->getSamplesLength() == 3);

  int samplesRet [] = {0, 0, 0};
  o2->getSamples(samplesRet);
  
  fail_unless(samplesRet[0] == 1);
  fail_unless(samplesRet[1] == 2);
  fail_unless(samplesRet[2] == 3);

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_TransformationComponents_copyConstructor )
{
  TransformationComponents* o1=new TransformationComponents(GNS);
  double points [] = {1,2,3};
  o1->setComponents(points, 3);
  
  fail_unless(o1->getComponentsLength() == 3);

  TransformationComponents* o2=new TransformationComponents(*o1);

  fail_unless(o2->getComponentsLength() == 3);
  
  double componentsRet [] = {0, 0, 0};
  o2->getComponents(componentsRet);
  
  fail_unless(util_isEqual(componentsRet[0], 1));
  fail_unless(util_isEqual(componentsRet[1], 2));
  fail_unless(util_isEqual(componentsRet[2], 3));

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_TransformationComponents_assignmentOperator )
{
  TransformationComponents* o1=new TransformationComponents(GNS);
  double points [] = {1,2,3};
  o1->setComponents(points, 3);
  
  fail_unless(o1->getComponentsLength() == 3);
  
  TransformationComponents* o2 = new TransformationComponents(GNS);;
  (*o2)=*o1;

  fail_unless(o2->getComponentsLength() == 3);

  double componentsRet [] = {0, 0, 0};
  o2->getComponents(componentsRet);
  
  fail_unless(util_isEqual(componentsRet[0], 1));
  fail_unless(util_isEqual(componentsRet[1], 2));
  fail_unless(util_isEqual(componentsRet[2], 3));

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_TransformationComponents_clone )
{
  TransformationComponents* o1=new TransformationComponents(GNS);
  double points [] = {1,2,3};
  o1->setComponents(points, 3);
  
  fail_unless(o1->getComponentsLength() == 3);

  TransformationComponents* o2=o1->clone();
 
  fail_unless(o2->getComponentsLength() == 3);

  double componentsRet [] = {0, 0, 0};
  o2->getComponents(componentsRet);
  
  fail_unless(util_isEqual(componentsRet[0], 1));
  fail_unless(util_isEqual(componentsRet[1], 2));
  fail_unless(util_isEqual(componentsRet[2], 3));

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

   tcase_add_checked_fixture( tcase,
                             SpatialCopyAndClone_setup,
                             SpatialCopyAndClone_teardown );

  tcase_add_test( tcase, test_PolygonObject_copyConstructor );
  tcase_add_test( tcase, test_PolygonObject_assignmentOperator );
  tcase_add_test( tcase, test_PolygonObject_clone );
  tcase_add_test( tcase, test_ParametricObject_copyConstructor );
  tcase_add_test( tcase, test_ParametricObject_assignmentOperator );
  tcase_add_test( tcase, test_ParametricObject_clone );
  tcase_add_test( tcase, test_SampledField_copyConstructor );
  tcase_add_test( tcase, test_SampledField_assignmentOperator );
  tcase_add_test( tcase, test_SampledField_clone );
  tcase_add_test( tcase, test_TransformationComponents_copyConstructor );
  tcase_add_test( tcase, test_TransformationComponents_assignmentOperator );
  tcase_add_test( tcase, test_TransformationComponents_clone );

  suite_add_tcase(suite, tcase);

  return suite;
}
CK_CPPEND

