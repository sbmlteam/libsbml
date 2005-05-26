/**
 * Filename    : TestLayoutFormatter.cpp
 * Description : Unit tests for LayoutFormatter
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
#include <iostream>
#include <check.h>

#include <common/common.h>

#include "LayoutFormatter.h"


BEGIN_C_DECLS

#ifndef USE_EXPAT
XERCES_CPP_NAMESPACE_USE
#endif /* !USE_EXPAT */

static MemBufFormatTarget *target;
static LayoutFormatter *LF;

void
LayoutFormatterTest_setup (void)
{
  try
  {
    XML_PLATFORM_UTILS_INIT();
  }
  catch (...)
  {
    fail("XMLPlatformUtils::Initialize() threw an Exception.");
  }

    target    = new MemBufFormatTarget();
    LF = new(std::nothrow )LayoutFormatter(target);

    if (LF == NULL)
    {
        fail("new(std::nothrow) LayoutFormatter() returned a NULL pointer.");
    }

}

void 
LayoutFormatterTest_teardown (void)
{
    delete LF;
    delete target;
}


Suite *
create_suite_LayoutFormatter (void)
{
  Suite *suite = suite_create("LayoutFormatter");
  TCase *tcase = tcase_create("LayoutFormatter");

  tcase_add_checked_fixture( tcase,
                             LayoutFormatterTest_setup,
                             LayoutFormatterTest_teardown );


  suite_add_tcase(suite, tcase);

  return suite;
}



END_C_DECLS
