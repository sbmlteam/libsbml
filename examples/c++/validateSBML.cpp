/**
 * \file    validateSBML.cpp
 * \brief   Validates an SBML file against the appropriate schema
 * \author  Sarah Keating and Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
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
 *     Sarah Keating
 *
 *     The SBML Team
 *     STRI
 *     University of Hertfordshire
 *     Hatfield, UK
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/common/common.h"
#include "sbml/SBMLReader.h"
#include "sbml/SBMLTypes.h"

#include "util.h"

#include <iostream>


using namespace std;


int
main (int argc, char *argv[])
{
  const char* filename;

  unsigned long start, stop, size;
  unsigned int  errors = 0;

  SBMLDocument *d;


  if (argc != 2)
  {
    cout << endl << " usage: validateSBML <filename>" << endl << endl;
    return 1;
  }


  SBMLReader sr;
  sr.setSchemaValidationLevel(XML_SCHEMA_VALIDATION_BASIC);

  sr.setSchemaFilenameL1v1("sbml-l1v1.xsd");
  sr.setSchemaFilenameL1v2("sbml-l1v2.xsd");
  sr.setSchemaFilenameL2v1("sbml-l2v1.xsd");

  filename = argv[1];

  start = getCurrentMillis();
  d     = sr.readSBML(filename);
  stop  = getCurrentMillis();

  errors  = d->getNumWarnings() + d->getNumErrors() + d->getNumFatals();
  errors += d->checkConsistency();

  size = getFileSize(filename);

  cout << endl;
  cout << "        filename: " << filename     << endl;
  cout << "       file size: " << size         << endl;
  cout << "  read time (ms): " << stop - start << endl;
  cout << "        error(s): " << errors       << endl;

  if (errors > 0)
  {
    d->printWarnings(cout);
    d->printErrors  (cout);
    d->printFatals  (cout);
  }

  cout << endl;

  delete d;
  return errors;
}
