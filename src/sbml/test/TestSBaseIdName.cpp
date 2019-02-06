/**
 * \file    TestSBaseIdName.cpp
 * \brief   SBase unit tests
 * \author  Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
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
#include <sbml/common/extern.h>

#include <sbml/SBase.h>
#include <sbml/Model.h>
#include <sbml/SBMLTypes.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLWriter.h>
#include <sbml/annotation/CVTerm.h>
#include <sbml/annotation/ModelHistory.h>
#include <sbml/annotation/ModelCreator.h>
#include <sbml/annotation/Date.h>

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE


/*
 * We create a lot of strings in this file, for testing, and we don't 
 * do what this warning tries to help with, so we shut it up just
 * for this file.
 */
#ifdef __GNUC__ 
#pragma GCC diagnostic ignored "-Wwrite-strings"
#endif

BEGIN_C_DECLS

static SBase *S31;
static SBase *S32;
static SBase *E31;
static SBase *E32;
static SBase *U31;
static SBase *U32;
static AssignmentRule *AR31;
static AssignmentRule *AR32;
static EventAssignment *EA31;
static EventAssignment *EA32;


void
SBaseIdNameTest_setup (void)
{
  S31 = new(std::nothrow) Species(3, 1);

  if (S31 == NULL)
  {
    fail("'new(std::nothrow) SBase;' returned a NULL pointer.");
  }

  S32 = new(std::nothrow) Species(3, 2);

  if (S32 == NULL)
  {
    fail("'new(std::nothrow) SBase;' returned a NULL pointer.");
  }

  E31 = new(std::nothrow) Event(3, 1);

  if (E31 == NULL)
  {
    fail("'new(std::nothrow) SBase;' returned a NULL pointer.");
  }

  E32 = new(std::nothrow) Event(3, 2);

  if (E32 == NULL)
  {
    fail("'new(std::nothrow) SBase;' returned a NULL pointer.");
  }

  U31 = new(std::nothrow) Unit(3, 1);

  if (U31 == NULL)
  {
    fail("'new(std::nothrow) SBase;' returned a NULL pointer.");
  }

  U32 = new(std::nothrow) Unit(3, 2);

  if (U32 == NULL)
  {
    fail("'new(std::nothrow) SBase;' returned a NULL pointer.");
  }

  AR31 = new(std::nothrow) AssignmentRule(3, 1);

  if (AR31 == NULL)
  {
    fail("'new(std::nothrow) SBase;' returned a NULL pointer.");
  }

  AR32 = new(std::nothrow) AssignmentRule(3, 2);

  if (AR32 == NULL)
  {
    fail("'new(std::nothrow) SBase;' returned a NULL pointer.");
  }

  EA31 = new(std::nothrow) EventAssignment(3, 1);

  if (EA31 == NULL)
  {
    fail("'new(std::nothrow) SBase;' returned a NULL pointer.");
  }

  EA32 = new(std::nothrow) EventAssignment(3, 2);

  if (EA32 == NULL)
  {
    fail("'new(std::nothrow) SBase;' returned a NULL pointer.");
  }
}


void
SBaseIdNameTest_teardown (void)
{
  delete S31;
  delete S32;
  delete E31;
  delete E32;
  delete U31;
  delete U32;
  delete AR31;
  delete AR32;
  delete EA31;
  delete EA32;
}


START_TEST (test_SBase_setId_1)
{
  const char *id = "x12345";

  int i = S31->setId(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(S31->isSetId());
  fail_unless(S31->getId() == id);
  fail_unless(S31->isSetIdAttribute());
  fail_unless(S31->getIdAttribute() == id);

  i = S31->unsetId();

  fail_unless(i == LIBSBML_OPERATION_FAILED);
  fail_unless(S31->isSetId());
  fail_unless(S31->getId() == id);
  fail_unless(S31->isSetIdAttribute());
  fail_unless(S31->getIdAttribute() == id);
}
END_TEST


START_TEST (test_SBase_setIdAttribute_1)
{
  const char *id = "x12345";

  int i = S31->setIdAttribute(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(S31->isSetId());
  fail_unless(S31->getId() == id);
  fail_unless(S31->isSetIdAttribute());
  fail_unless(S31->getIdAttribute() == id);

  i = S31->unsetIdAttribute();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!S31->isSetId());
  fail_unless(S31->getId() == "");
  fail_unless(!S31->isSetIdAttribute());
  fail_unless(S31->getIdAttribute() == "");
}
END_TEST


START_TEST (test_SBase_setName_1)
{
  const char *name = "x12345";


  SBase_setName(S31, name);

  fail_unless( !strcmp(SBase_getName(S31), name), NULL );
  fail_unless( SBase_isSetName(S31)      , NULL );

  if (SBase_getName(S31) == name)
  {
    fail("SBase_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SBase_setName(S31, SBase_getName(S31));
  fail_unless( !strcmp(SBase_getName(S31), name), NULL );

  SBase_setName(S31, NULL);
  fail_unless( !SBase_isSetName(S31), NULL );

  if (SBase_getName(S31) != NULL)
  {
    fail("SBase_setName(S31, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_SBase_setId_2)
{
  const char *id = "x12345";

  int i = S32->setId(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(S32->isSetId());
  fail_unless(S32->getId() == id);
  fail_unless(S32->isSetIdAttribute());
  fail_unless(S32->getIdAttribute() == id);

  i = S32->unsetId();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!S32->isSetId());
  fail_unless(S32->getId() == "");
  fail_unless(!S32->isSetIdAttribute());
  fail_unless(S32->getIdAttribute() == "");
}
END_TEST


START_TEST (test_SBase_setIdAttribute_2)
{
  const char *id = "x12345";

  int i = S32->setIdAttribute(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(S32->isSetId());
  fail_unless(S32->getId() == id);
  fail_unless(S32->isSetIdAttribute());
  fail_unless(S32->getIdAttribute() == id);

  i = S32->unsetIdAttribute();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!S32->isSetId());
  fail_unless(S32->getId() == "");
  fail_unless(!S32->isSetIdAttribute());
  fail_unless(S32->getIdAttribute() == "");
}
END_TEST


START_TEST (test_SBase_setName_2)
{
  const char *name = "x12345";


  SBase_setName(S32, name);

  fail_unless( !strcmp(SBase_getName(S32), name), NULL );
  fail_unless( SBase_isSetName(S32)      , NULL );

  if (SBase_getName(S32) == name)
  {
    fail("SBase_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SBase_setName(S32, SBase_getName(S32));
  fail_unless( !strcmp(SBase_getName(S32), name), NULL );

  SBase_setName(S32, NULL);
  fail_unless( !SBase_isSetName(S32), NULL );

  if (SBase_getName(S32) != NULL)
  {
    fail("SBase_setName(S32, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_SBase_setId_3)
{
  const char *id = "x12345";

  int i = E31->setId(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(E31->isSetId());
  fail_unless(E31->getId() == id);
  fail_unless(E31->isSetIdAttribute());
  fail_unless(E31->getIdAttribute() == id);

  i = E31->unsetId();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!E31->isSetId());
  fail_unless(E31->getId() == "");
  fail_unless(!E31->isSetIdAttribute());
  fail_unless(E31->getIdAttribute() == "");
}
END_TEST


START_TEST (test_SBase_setIdAttribute_3)
{
  const char *id = "x12345";

  int i = E31->setIdAttribute(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(E31->isSetId());
  fail_unless(E31->getId() == id);
  fail_unless(E31->isSetIdAttribute());
  fail_unless(E31->getIdAttribute() == id);

  i = E31->unsetIdAttribute();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!E31->isSetId());
  fail_unless(E31->getId() == "");
  fail_unless(!E31->isSetIdAttribute());
  fail_unless(E31->getIdAttribute() == "");
}
END_TEST


START_TEST (test_SBase_setName_3)
{
  const char *name = "x12345";


  SBase_setName(E31, name);

  fail_unless( !strcmp(SBase_getName(E31), name), NULL );
  fail_unless( SBase_isSetName(E31)      , NULL );

  if (SBase_getName(E31) == name)
  {
    fail("SBase_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SBase_setName(E31, SBase_getName(E31));
  fail_unless( !strcmp(SBase_getName(E31), name), NULL );

  SBase_setName(E31, NULL);
  fail_unless( !SBase_isSetName(E31), NULL );

  if (SBase_getName(E31) != NULL)
  {
    fail("SBase_setName(E31, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_SBase_setId_4)
{
  const char *id = "x12345";

  int i = E32->setId(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(E32->isSetId());
  fail_unless(E32->getId() == id);
  fail_unless(E32->isSetIdAttribute());
  fail_unless(E32->getIdAttribute() == id);

  i = E32->unsetId();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!E32->isSetId());
  fail_unless(E32->getId() == "");
  fail_unless(!E32->isSetIdAttribute());
  fail_unless(E32->getIdAttribute() == "");
}
END_TEST


START_TEST (test_SBase_setIdAttribute_4)
{
  const char *id = "x12345";

  int i = E32->setIdAttribute(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(E32->isSetId());
  fail_unless(E32->getId() == id);
  fail_unless(E32->isSetIdAttribute());
  fail_unless(E32->getIdAttribute() == id);

  i = E32->unsetIdAttribute();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!E32->isSetId());
  fail_unless(E32->getId() == "");
  fail_unless(!E32->isSetIdAttribute());
  fail_unless(E32->getIdAttribute() == "");
}
END_TEST


START_TEST (test_SBase_setName_4)
{
  const char *name = "x12345";


  SBase_setName(E32, name);

  fail_unless( !strcmp(SBase_getName(E32), name), NULL );
  fail_unless( SBase_isSetName(E32)      , NULL );

  if (SBase_getName(E32) == name)
  {
    fail("SBase_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SBase_setName(E32, SBase_getName(E32));
  fail_unless( !strcmp(SBase_getName(E32), name), NULL );

  SBase_setName(E32, NULL);
  fail_unless( !SBase_isSetName(E32), NULL );

  if (SBase_getName(E32) != NULL)
  {
    fail("SBase_setName(E32, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_SBase_setId_5)
{
  const char *id = "x12345";

  int i = U31->setId(id);

  fail_unless(i == LIBSBML_UNEXPECTED_ATTRIBUTE);
  fail_unless(!U31->isSetId());
  fail_unless(U31->getId() == "");
  fail_unless(!U31->isSetIdAttribute());
  fail_unless(U31->getIdAttribute() == "");

  i = U31->unsetId();

  fail_unless(i == LIBSBML_OPERATION_FAILED);
  fail_unless(!U31->isSetId());
  fail_unless(U31->getId() == "");
  fail_unless(!U31->isSetIdAttribute());
  fail_unless(U31->getIdAttribute() == "");
}
END_TEST


START_TEST (test_SBase_setIdAttribute_5)
{
  const char *id = "x12345";

  int i = U31->setIdAttribute(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!U31->isSetId());
  fail_unless(U31->getId() == "");
  fail_unless(U31->isSetIdAttribute());
  fail_unless(U31->getIdAttribute() == id);

  i = U31->unsetIdAttribute();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!U31->isSetId());
  fail_unless(U31->getId() == "");
  fail_unless(!U31->isSetIdAttribute());
  fail_unless(U31->getIdAttribute() == "");
}
END_TEST


START_TEST (test_SBase_setName_5)
{
  const char *name = "x12345";

  int i = SBase_setName(U31, name);

  fail_unless(i == LIBSBML_UNEXPECTED_ATTRIBUTE);
  fail_unless( !SBase_isSetName(U31)      , NULL );
  fail_unless(SBase_getName(U31) == NULL);
}
END_TEST


START_TEST (test_SBase_setId_6)
{
  const char *id = "x12345";

  int i = U32->setId(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(U32->isSetId());
  fail_unless(U32->getId() == id);
  fail_unless(U32->isSetIdAttribute());
  fail_unless(U32->getIdAttribute() == id);

  i = U32->unsetId();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!U32->isSetId());
  fail_unless(U32->getId() == "");
  fail_unless(!U32->isSetIdAttribute());
  fail_unless(U32->getIdAttribute() == "");
}
END_TEST


START_TEST (test_SBase_setIdAttribute_6)
{
  const char *id = "x12345";

  int i = U32->setIdAttribute(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(U32->isSetId());
  fail_unless(U32->getId() == id);
  fail_unless(U32->isSetIdAttribute());
  fail_unless(U32->getIdAttribute() == id);

  i = U32->unsetIdAttribute();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!U32->isSetId());
  fail_unless(U32->getId() == "");
  fail_unless(!U32->isSetIdAttribute());
  fail_unless(U32->getIdAttribute() == "");
}
END_TEST


START_TEST (test_SBase_setName_6)
{
  const char *name = "x12345";


  SBase_setName(U32, name);

  fail_unless( !strcmp(SBase_getName(U32), name), NULL );
  fail_unless( SBase_isSetName(U32)      , NULL );

  if (SBase_getName(U32) == name)
  {
    fail("SBase_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SBase_setName(U32, SBase_getName(U32));
  fail_unless( !strcmp(SBase_getName(U32), name), NULL );

  SBase_setName(U32, NULL);
  fail_unless( !SBase_isSetName(U32), NULL );

  if (SBase_getName(U32) != NULL)
  {
    fail("SBase_setName(U32, NULL) did not clear string.");
  }
}
END_TEST

  
START_TEST (test_SBase_setId_7)
{
  const char *id = "x12345";

  int i = AR31->setId(id);

  fail_unless(i == LIBSBML_UNEXPECTED_ATTRIBUTE);
  fail_unless(!AR31->isSetId());
  fail_unless(AR31->getId() == "");
  fail_unless(!AR31->isSetIdAttribute());
  fail_unless(AR31->getIdAttribute() == "");
  fail_unless(!AR31->isSetVariable());
  fail_unless(AR31->getVariable() == "");

  i = AR31->unsetId();

  fail_unless(i == LIBSBML_OPERATION_FAILED);
  fail_unless(!AR31->isSetId());
  fail_unless(AR31->getId() == "");
  fail_unless(!AR31->isSetIdAttribute());
  fail_unless(AR31->getIdAttribute() == "");
  fail_unless(!AR31->isSetVariable());
  fail_unless(AR31->getVariable() == "");
}
END_TEST


START_TEST (test_SBase_setIdAttribute_7)
{
  const char *id = "x12345";

  int i = AR31->setIdAttribute(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!AR31->isSetId());
  fail_unless(AR31->getId() == "");
  fail_unless(AR31->isSetIdAttribute());
  fail_unless(AR31->getIdAttribute() == id);
  fail_unless(!AR31->isSetVariable());
  fail_unless(AR31->getVariable() == "");

  i = AR31->unsetIdAttribute();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!AR31->isSetId());
  fail_unless(AR31->getId() == "");
  fail_unless(!AR31->isSetIdAttribute());
  fail_unless(AR31->getIdAttribute() == "");
  fail_unless(!AR31->isSetVariable());
  fail_unless(AR31->getVariable() == "");
}
END_TEST


START_TEST (test_SBase_setName_7)
{
  const char *name = "x12345";

  int i = SBase_setName(AR31, name);

  fail_unless(i == LIBSBML_UNEXPECTED_ATTRIBUTE);
  fail_unless( !SBase_isSetName(AR31)      , NULL );
  fail_unless(SBase_getName(AR31) == NULL);
}
END_TEST


START_TEST (test_SBase_setVariable_7)
{
  const char *id = "x12345";

  int i = AR31->setVariable(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(AR31->isSetId());
  fail_unless(AR31->getId() == id);
  fail_unless(!AR31->isSetIdAttribute());
  fail_unless(AR31->getIdAttribute() == "");
  fail_unless(AR31->isSetVariable());
  fail_unless(AR31->getVariable() == id);

  i = AR31->unsetVariable();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!AR31->isSetId());
  fail_unless(AR31->getId() == "");
  fail_unless(!AR31->isSetIdAttribute());
  fail_unless(AR31->getIdAttribute() == "");
  fail_unless(!AR31->isSetVariable());
  fail_unless(AR31->getVariable() == "");
}
END_TEST


START_TEST (test_SBase_setId_8)
{
  const char *id = "x12345";

  int i = AR32->setId(id);

  fail_unless(i == LIBSBML_USE_ID_ATTRIBUTE_FUNCTION);
  fail_unless(!AR32->isSetId());
  fail_unless(AR32->getId() == "");
  fail_unless(!AR32->isSetIdAttribute());
  fail_unless(AR32->getIdAttribute() == "");
  fail_unless(!AR32->isSetVariable());
  fail_unless(AR32->getVariable() == "");

  i = AR32->unsetId();

  fail_unless(i == LIBSBML_USE_ID_ATTRIBUTE_FUNCTION);
  fail_unless(!AR32->isSetId());
  fail_unless(AR32->getId() == "");
  fail_unless(!AR32->isSetIdAttribute());
  fail_unless(AR32->getIdAttribute() == "");
  fail_unless(!AR32->isSetVariable());
  fail_unless(AR32->getVariable() == "");
}
END_TEST


START_TEST (test_SBase_setIdAttribute_8)
{
  const char *id = "x12345";

  int i = AR32->setIdAttribute(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!AR32->isSetId());
  fail_unless(AR32->getId() == "");
  fail_unless(AR32->isSetIdAttribute());
  fail_unless(AR32->getIdAttribute() == id);
  fail_unless(!AR32->isSetVariable());
  fail_unless(AR32->getVariable() == "");

  i = AR32->unsetIdAttribute();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!AR32->isSetId());
  fail_unless(AR32->getId() == "");
  fail_unless(!AR32->isSetIdAttribute());
  fail_unless(AR32->getIdAttribute() == "");
  fail_unless(!AR32->isSetVariable());
  fail_unless(AR32->getVariable() == "");
}
END_TEST


START_TEST (test_SBase_setName_8)
{
  const char *name = "x12345";


  SBase_setName(AR32, name);

  fail_unless( !strcmp(SBase_getName(AR32), name), NULL );
  fail_unless( SBase_isSetName(AR32)      , NULL );

  if (SBase_getName(AR32) == name)
  {
    fail("SBase_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SBase_setName(AR32, SBase_getName(AR32));
  fail_unless( !strcmp(SBase_getName(AR32), name), NULL );

  SBase_setName(AR32, NULL);
  fail_unless( !SBase_isSetName(AR32), NULL );

  if (SBase_getName(AR32) != NULL)
  {
    fail("SBase_setName(AR32, NULL) did not clear string.");
  }
}
END_TEST

  
START_TEST (test_SBase_setVariable_8)
{
  const char *id = "x12345";

  int i = AR31->setVariable(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(AR31->isSetId());
  fail_unless(AR31->getId() == id);
  fail_unless(!AR31->isSetIdAttribute());
  fail_unless(AR31->getIdAttribute() == "");
  fail_unless(AR31->isSetVariable());
  fail_unless(AR31->getVariable() == id);

  i = AR31->unsetVariable();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!AR31->isSetId());
  fail_unless(AR31->getId() == "");
  fail_unless(!AR31->isSetIdAttribute());
  fail_unless(AR31->getIdAttribute() == "");
  fail_unless(!AR31->isSetVariable());
  fail_unless(AR31->getVariable() == "");
}
END_TEST


START_TEST (test_SBase_setId_9)
{
  const char *id = "x12345";

  int i = EA31->setId(id);

  fail_unless(i == LIBSBML_UNEXPECTED_ATTRIBUTE);
  fail_unless(!EA31->isSetId());
  fail_unless(EA31->getId() == "");
  fail_unless(!EA31->isSetIdAttribute());
  fail_unless(EA31->getIdAttribute() == "");
  fail_unless(!EA31->isSetVariable());
  fail_unless(EA31->getVariable() == "");

  i = EA31->unsetId();

  fail_unless(i == LIBSBML_OPERATION_FAILED);
  fail_unless(!EA31->isSetId());
  fail_unless(EA31->getId() == "");
  fail_unless(!EA31->isSetIdAttribute());
  fail_unless(EA31->getIdAttribute() == "");
  fail_unless(!EA31->isSetVariable());
  fail_unless(EA31->getVariable() == "");
}
END_TEST


START_TEST (test_SBase_setIdAttribute_9)
{
  const char *id = "x12345";

  int i = EA31->setIdAttribute(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!EA31->isSetId());
  fail_unless(EA31->getId() == "");
  fail_unless(EA31->isSetIdAttribute());
  fail_unless(EA31->getIdAttribute() == id);
  fail_unless(!EA31->isSetVariable());
  fail_unless(EA31->getVariable() == "");

  i = EA31->unsetIdAttribute();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!EA31->isSetId());
  fail_unless(EA31->getId() == "");
  fail_unless(!EA31->isSetIdAttribute());
  fail_unless(EA31->getIdAttribute() == "");
  fail_unless(!EA31->isSetVariable());
  fail_unless(EA31->getVariable() == "");
}
END_TEST


START_TEST (test_SBase_setName_9)
{
  const char *name = "x12345";

  int i = SBase_setName(EA31, name);

  fail_unless(i == LIBSBML_UNEXPECTED_ATTRIBUTE);
  fail_unless( !SBase_isSetName(EA31)      , NULL );
  fail_unless(SBase_getName(EA31) == NULL);
}
END_TEST


START_TEST (test_SBase_setVariable_9)
{
  const char *id = "x12345";

  int i = EA31->setVariable(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(EA31->isSetId());
  fail_unless(EA31->getId() == id);
  fail_unless(!EA31->isSetIdAttribute());
  fail_unless(EA31->getIdAttribute() == "");
  fail_unless(EA31->isSetVariable());
  fail_unless(EA31->getVariable() == id);

  i = EA31->unsetVariable();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!EA31->isSetId());
  fail_unless(EA31->getId() == "");
  fail_unless(!EA31->isSetIdAttribute());
  fail_unless(EA31->getIdAttribute() == "");
  fail_unless(!EA31->isSetVariable());
  fail_unless(EA31->getVariable() == "");
}
END_TEST


START_TEST (test_SBase_setId_10)
{
  const char *id = "x12345";

  int i = EA32->setId(id);

  fail_unless(i == LIBSBML_USE_ID_ATTRIBUTE_FUNCTION);
  fail_unless(!EA32->isSetId());
  fail_unless(EA32->getId() == "");
  fail_unless(!EA32->isSetIdAttribute());
  fail_unless(EA32->getIdAttribute() == "");
  fail_unless(!EA32->isSetVariable());
  fail_unless(EA32->getVariable() == "");

  i = EA32->unsetId();

  fail_unless(i == LIBSBML_USE_ID_ATTRIBUTE_FUNCTION);
  fail_unless(!EA32->isSetId());
  fail_unless(EA32->getId() == "");
  fail_unless(!EA32->isSetIdAttribute());
  fail_unless(EA32->getIdAttribute() == "");
  fail_unless(!EA32->isSetVariable());
  fail_unless(EA32->getVariable() == "");
}
END_TEST


START_TEST (test_SBase_setIdAttribute_10)
{
  const char *id = "x12345";

  int i = EA32->setIdAttribute(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!EA32->isSetId());
  fail_unless(EA32->getId() == "");
  fail_unless(EA32->isSetIdAttribute());
  fail_unless(EA32->getIdAttribute() == id);
  fail_unless(!EA32->isSetVariable());
  fail_unless(EA32->getVariable() == "");

  i = EA32->unsetIdAttribute();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!EA32->isSetId());
  fail_unless(EA32->getId() == "");
  fail_unless(!EA32->isSetIdAttribute());
  fail_unless(EA32->getIdAttribute() == "");
  fail_unless(!EA32->isSetVariable());
  fail_unless(EA32->getVariable() == "");
}
END_TEST


START_TEST (test_SBase_setName_10)
{
  const char *name = "x12345";


  SBase_setName(EA32, name);

  fail_unless( !strcmp(SBase_getName(EA32), name), NULL );
  fail_unless( SBase_isSetName(EA32)      , NULL );

  if (SBase_getName(EA32) == name)
  {
    fail("SBase_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SBase_setName(EA32, SBase_getName(EA32));
  fail_unless( !strcmp(SBase_getName(EA32), name), NULL );

  SBase_setName(EA32, NULL);
  fail_unless( !SBase_isSetName(EA32), NULL );

  if (SBase_getName(EA32) != NULL)
  {
    fail("SBase_setName(EA32, NULL) did not clear string.");
  }
}
END_TEST

  
START_TEST (test_SBase_setVariable_10)
{
  const char *id = "x12345";

  int i = EA31->setVariable(id);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(EA31->isSetId());
  fail_unless(EA31->getId() == id);
  fail_unless(!EA31->isSetIdAttribute());
  fail_unless(EA31->getIdAttribute() == "");
  fail_unless(EA31->isSetVariable());
  fail_unless(EA31->getVariable() == id);

  i = EA31->unsetVariable();

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(!EA31->isSetId());
  fail_unless(EA31->getId() == "");
  fail_unless(!EA31->isSetIdAttribute());
  fail_unless(EA31->getIdAttribute() == "");
  fail_unless(!EA31->isSetVariable());
  fail_unless(EA31->getVariable() == "");
}
END_TEST


Suite *
create_suite_SBase_IdName (void)
{
  Suite *suite = suite_create("SBaseIdName");
  TCase *tcase = tcase_create("SBaseIdName");


  tcase_add_checked_fixture(tcase, SBaseIdNameTest_setup, SBaseIdNameTest_teardown);

  tcase_add_test(tcase, test_SBase_setId_1     );
  tcase_add_test(tcase, test_SBase_setIdAttribute_1     );
  tcase_add_test(tcase, test_SBase_setName_1     );

  tcase_add_test(tcase, test_SBase_setId_2     );
  tcase_add_test(tcase, test_SBase_setIdAttribute_2     );
  tcase_add_test(tcase, test_SBase_setName_2     );

  tcase_add_test(tcase, test_SBase_setId_3     );
  tcase_add_test(tcase, test_SBase_setIdAttribute_3     );
  tcase_add_test(tcase, test_SBase_setName_3     );

  tcase_add_test(tcase, test_SBase_setId_4     );
  tcase_add_test(tcase, test_SBase_setIdAttribute_4     );
  tcase_add_test(tcase, test_SBase_setName_4     );

  tcase_add_test(tcase, test_SBase_setId_5     );
  tcase_add_test(tcase, test_SBase_setIdAttribute_5     );
  tcase_add_test(tcase, test_SBase_setName_5     );

  tcase_add_test(tcase, test_SBase_setId_6     );
  tcase_add_test(tcase, test_SBase_setIdAttribute_6     );
  tcase_add_test(tcase, test_SBase_setName_6     );

  tcase_add_test(tcase, test_SBase_setId_7     );
  tcase_add_test(tcase, test_SBase_setIdAttribute_7     );
  tcase_add_test(tcase, test_SBase_setName_7     );
  tcase_add_test(tcase, test_SBase_setVariable_7     );

  tcase_add_test(tcase, test_SBase_setId_8     );
  tcase_add_test(tcase, test_SBase_setIdAttribute_8     );
  tcase_add_test(tcase, test_SBase_setName_8     );
  tcase_add_test(tcase, test_SBase_setVariable_8     );

  tcase_add_test(tcase, test_SBase_setId_9     );
  tcase_add_test(tcase, test_SBase_setIdAttribute_9     );
  tcase_add_test(tcase, test_SBase_setName_9     );
  tcase_add_test(tcase, test_SBase_setVariable_9     );

  tcase_add_test(tcase, test_SBase_setId_10     );
  tcase_add_test(tcase, test_SBase_setIdAttribute_10     );
  tcase_add_test(tcase, test_SBase_setName_10     );
  tcase_add_test(tcase, test_SBase_setVariable_10     );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
