//
// Filename    : TestStyle.cpp
// Description : Tests for the Style class
// Organization: University of Heidelberg
// Created     : 2009-09-30
//
// Copyright 2008 University of Heidelberg
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published
// by the Free Software Foundation; either version 2.1 of the License, or
// any later version.
//
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
// MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
// documentation provided hereunder is on an "as is" basis, and the
// University of Heidelberg have no obligations to
// provide maintenance, support, updates, enhancements or modifications.
// In no event shall the University of Heidelberg be
// liable to any party for direct, indirect, special, incidental or
// consequential damages, including lost profits, arising out of the use of
// this software and its documentation, even if the University of 
// Heidelberg have been advised of the possibility of such
// damage.  See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// The original code contained here was initially developed by:
//
//     Ralph Gauges
//     BIOQUANT/BQ0018
//     Im Neuenheimer Feld 267
//     69120 Heidelberg
//     Germany
//
//     mailto:ralph.gauges@bioquant.uni-heidelberg.de
//
// Contributor(s):



#include <sbml/common/common.h>
#include <sbml/common/extern.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLInputStream.h>


#include <Style.h>
#include <LocalStyle.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static Style *S;
static RenderPkgNamespaces *renderns;

void
StyleTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    S = new (std::nothrow) LocalStyle(renderns);

    if (S == NULL)
    {
        fail("new(std::nothrow)LocalStyle(renderns) returned a NULL pointer.");
    }
}

void 
StyleTest_teardown (void)
{
    delete S;
    delete renderns;
}

START_TEST (test_Style_id )
{
    fail_unless(!S->isSetId());
    fail_unless(S->getId() == "");
    S->setId("MyStyle");
    fail_unless(S->isSetId());
    fail_unless(S->getId() == "MyStyle");
    S->unsetId();
    fail_unless(!S->isSetId());
    fail_unless(S->getId() == "");
}
END_TEST 

START_TEST (test_Style_name )
{
    fail_unless(!S->isSetName());
    fail_unless(S->getName() == "");
    S->setName("MyStyle");
    fail_unless(S->isSetName());
    fail_unless(S->getName() == "MyStyle");
    S->unsetName();
    fail_unless(!S->isSetName());
    fail_unless(S->getName() == "");
}
END_TEST 

START_TEST ( test_Style_rolelist )
{
    fail_unless ( S->getNumRoles() == 0 );
    S->addRole("ROLE_1");
    fail_unless ( S->getNumRoles() == 1 );
    S->addRole("ROLE_2");
    fail_unless ( S->getNumRoles() == 2 );
    S->addRole("ROLE_3");
    fail_unless ( S->getNumRoles() == 3 );
    S->addRole("ROLE_4");
    fail_unless ( S->getNumRoles() == 4 );
    S->removeRole("ROLE_3");
    fail_unless ( S->getNumRoles() == 3 );
    fail_unless( S->isInRoleList("ROLE_1"));
    fail_unless( S->isInRoleList("ROLE_2"));
    fail_unless( S->isInRoleList("ROLE_4"));
    std::set<std::string> s;
    s.insert("role_a");
    s.insert("role_b");
    s.insert("role_c");
    s.insert("role_d");
    S->setRoleList(s);
    fail_unless ( S->getNumRoles() == 4 );
    fail_unless( S->isInRoleList("role_a"));
    fail_unless( S->isInRoleList("role_b"));
    fail_unless( S->isInRoleList("role_c"));
    fail_unless( S->isInRoleList("role_d"));
}
END_TEST

START_TEST ( test_Style_typelist )
{
    fail_unless ( S->getNumTypes() == 0 );
    S->addType("TYPE_1");
    fail_unless ( S->getNumTypes() == 1 );
    S->addType("TYPE_2");
    fail_unless ( S->getNumTypes() == 2 );
    S->addType("TYPE_3");
    fail_unless ( S->getNumTypes() == 3 );
    S->addType("TYPE_4");
    fail_unless ( S->getNumTypes() == 4 );
    S->removeType("TYPE_3");
    fail_unless ( S->getNumTypes() == 3 );
    fail_unless( S->isInTypeList("TYPE_1"));
    fail_unless( S->isInTypeList("TYPE_2"));
    fail_unless( S->isInTypeList("TYPE_4"));
    std::set<std::string> s;
    s.insert("type_a");
    s.insert("type_b");
    s.insert("type_c");
    s.insert("type_d");
    S->setTypeList(s);
    fail_unless ( S->getNumTypes() == 4 );
    fail_unless( S->isInTypeList("type_a"));
    fail_unless( S->isInTypeList("type_b"));
    fail_unless( S->isInTypeList("type_c"));
    fail_unless( S->isInTypeList("type_d"));
}
END_TEST


Suite *
create_suite_Style (void)
{
  Suite *suite = suite_create("Style");
  TCase *tcase = tcase_create("Style");


  tcase_add_checked_fixture( tcase,
                             StyleTest_setup,
                             StyleTest_teardown );

  tcase_add_test( tcase, test_Style_id       );
  tcase_add_test( tcase, test_Style_name     );
  tcase_add_test( tcase, test_Style_rolelist );
  tcase_add_test( tcase, test_Style_typelist );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
