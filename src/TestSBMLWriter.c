/**
 * Filename    : TestSBMLWriter.c
 * Description : SBMLWriter unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-03-22
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
 * Japan Science and Technology Corporation.
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
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/common.h"
#include "sbml/SBMLReader.h"
#include "sbml/SBMLWriter.h"


extern char *TestDataDirectory;


START_TEST (test_SBMLWriter_create)
{
  SBMLWriter_t *sw = SBMLWriter_create();


  fail_unless(sw->encoding == CHARACTER_ENCODING_UTF_8, NULL);

  SBMLWriter_free(sw);
}
END_TEST


START_TEST (test_SBMLWriter_free_NULL)
{
  SBMLWriter_free(NULL);
}
END_TEST


/*
START_TEST (test_SBMLWriter_writeSBML)
{
  char           *in  = safe_strcat(TestDataDirectory, "l1v1-branch.xml");
  char           *out = safe_strcat(TestDataDirectory, "l1v1-branch-out.xml");
  SBMLDocument_t *d   = readSBML(in);


  fail_unless( writeSBML(d, out) == 1, NULL );

  SBMLDocument_free(d);
  safe_free(in);
  safe_free(out);
}
END_TEST
*/


Suite *
create_suite_SBMLWriter (void) 
{ 
  Suite *suite = suite_create("SBMLWriter");
  TCase *tcase = tcase_create("SBMLWriter");
 

  tcase_add_test(tcase, test_SBMLWriter_create            );
  tcase_add_test(tcase, test_SBMLWriter_free_NULL         );
  /* tcase_add_test(tcase, test_SBMLWriter_writeSBML         ); */

  suite_add_tcase(suite, tcase);

  return suite;
}
